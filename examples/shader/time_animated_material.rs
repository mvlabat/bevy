//! A shader that uses dynamic data like the time since startup.
//!
//! This example uses a specialized pipeline.

use bevy::{
    core_pipeline::core_3d::{AlphaMask3d, Opaque3d, Transparent3d},
    ecs::system::{
        lifetimeless::{Read, SQuery, SRes},
        SystemParamItem,
    },
    pbr::{
        DrawMesh, MaterialPipeline, MaterialPipelineKey, MaterialProperties, MeshPipelineKey,
        MeshUniform, PreparedMaterial, RenderMaterials, SetMaterialBindGroup, SetMeshBindGroup,
        SetMeshViewBindGroup,
    },
    prelude::*,
    render::{
        extract_component::{
            ComponentUniforms, DynamicUniformIndex, ExtractComponent, ExtractComponentPlugin,
            UniformComponentPlugin,
        },
        mesh::MeshVertexBufferLayout,
        render_asset::{PrepareAssetLabel, RenderAssets},
        render_phase::{
            AddRenderCommand, DrawFunctions, EntityRenderCommand, RenderCommandResult, RenderPhase,
            SetItemPipeline, TrackedRenderPass,
        },
        render_resource::*,
        renderer::RenderDevice,
        texture::FallbackImage,
        view::{ComputedVisibility, ExtractedView, Msaa, Visibility, VisibleEntities},
        Extract, RenderApp, RenderStage,
    },
    utils::HashSet,
};
use std::{hash::Hash, marker::PhantomData};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(TimeAnimatedMaterialPlugin::<StandardMaterial>::default())
        .add_startup_system(setup)
        .add_system(update_time)
        .run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // cubes
    commands.spawn().insert_bundle((
        meshes.add(Mesh::from(shape::Cube { size: 1.0 })),
        Transform::from_xyz(0.0, 0.5, 0.0),
        GlobalTransform::default(),
        materials.add(StandardMaterial {
            base_color: Color::ORANGE_RED,
            alpha_mode: AlphaMode::Blend,
            ..default()
        }),
        Visibility::default(),
        ComputedVisibility::default(),
        AnimationTime { time: 0.0 },
    ));
    commands.spawn().insert_bundle((
        meshes.add(Mesh::from(shape::Cube { size: 1.5 })),
        Transform::from_xyz(-1.0, -1.5, 0.0),
        GlobalTransform::default(),
        materials.add(StandardMaterial {
            base_color: Color::OLIVE,
            alpha_mode: AlphaMode::Blend,
            ..default()
        }),
        Visibility::default(),
        ComputedVisibility::default(),
        AnimationTime { time: 3.5 },
    ));

    // light
    commands.spawn_bundle(PointLightBundle {
        point_light: PointLight {
            intensity: 1500.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(-4.0, 8.0, 4.0),
        ..default()
    });

    // camera
    commands.spawn_bundle(Camera3dBundle {
        transform: Transform::from_xyz(-2.0, 2.5, 5.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    });
}

fn update_time(time: Res<Time>, mut query: Query<&'static mut AnimationTime>) {
    for mut animation_time in query.iter_mut() {
        animation_time.time += time.delta_seconds();
    }
}

#[derive(Component, ShaderType, Clone, Debug)]
pub struct AnimationTime {
    pub time: f32,
}

fn extract_animation_time<M: Material>(
    mut commands: Commands,
    query: Extract<Query<(Entity, Option<&AnimationTime>), With<Handle<M>>>>,
) {
    for (entity, animation_time) in query.iter() {
        let animation_time = animation_time
            .cloned()
            .unwrap_or(AnimationTime { time: 0.0 });
        commands.get_or_spawn(entity).insert(animation_time);
    }
}

#[derive(Component)]
struct CustomMaterial;

#[derive(Default)]
pub struct TimeAnimatedMaterialPlugin<M: Material>(PhantomData<M>);

impl<M: Material> Plugin for TimeAnimatedMaterialPlugin<M>
where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    fn build(&self, app: &mut App) {
        app.add_plugin(ExtractComponentPlugin::<AnimationTime>::default())
            .add_plugin(UniformComponentPlugin::<AnimationTime>::default());

        app.sub_app_mut(RenderApp)
            .add_render_command::<Transparent3d, DrawAnimatedMaterial<M>>()
            .add_render_command::<Opaque3d, DrawAnimatedMaterial<M>>()
            .add_render_command::<AlphaMask3d, DrawAnimatedMaterial<M>>()
            .insert_resource(AnimationTimeBindGroup { bind_group: None })
            .init_resource::<AnimatedMaterialPipeline<M>>()
            .init_resource::<ExtractedMaterials<M>>()
            .init_resource::<RenderMaterials<M>>()
            .init_resource::<SpecializedMeshPipelines<AnimatedMaterialPipeline<M>>>()
            .add_system_to_stage(RenderStage::Extract, extract_materials::<M>)
            .add_system_to_stage(RenderStage::Extract, extract_animation_time::<M>)
            .add_system_to_stage(
                RenderStage::Prepare,
                prepare_materials::<M>.after(PrepareAssetLabel::PreAssetPrepare),
            )
            .add_system_to_stage(RenderStage::Queue, queue_time_animated_material_meshes::<M>)
            .add_system_to_stage(RenderStage::Queue, queue_time_bind_group::<M>);
    }
}

struct ExtractedMaterials<M: Material> {
    extracted: Vec<(Handle<M>, M)>,
    removed: Vec<Handle<M>>,
}

impl<M: Material> Default for ExtractedMaterials<M> {
    fn default() -> Self {
        Self {
            extracted: Default::default(),
            removed: Default::default(),
        }
    }
}

/// This system extracts all created or modified assets of the corresponding [`Material`] type
/// into the "render world".
fn extract_materials<M: Material>(
    mut commands: Commands,
    mut events: Extract<EventReader<AssetEvent<M>>>,
    assets: Extract<Res<Assets<M>>>,
) {
    let mut changed_assets = HashSet::default();
    let mut removed = Vec::new();
    for event in events.iter() {
        match event {
            AssetEvent::Created { handle } | AssetEvent::Modified { handle } => {
                changed_assets.insert(handle.clone_weak());
            }
            AssetEvent::Removed { handle } => {
                changed_assets.remove(handle);
                removed.push(handle.clone_weak());
            }
        }
    }

    let mut extracted_assets = Vec::new();
    for handle in changed_assets.drain() {
        if let Some(asset) = assets.get(&handle) {
            extracted_assets.push((handle, asset.clone()));
        }
    }

    commands.insert_resource(ExtractedMaterials {
        extracted: extracted_assets,
        removed,
    });
}

/// All [`Material`] values of a given type that should be prepared next frame.
pub struct PrepareNextFrameMaterials<M: Material> {
    assets: Vec<(Handle<M>, M)>,
}

impl<M: Material> Default for PrepareNextFrameMaterials<M> {
    fn default() -> Self {
        Self {
            assets: Default::default(),
        }
    }
}

/// This system prepares all assets of the corresponding [`Material`] type
/// which where extracted this frame for the GPU.
fn prepare_materials<M: Material>(
    mut prepare_next_frame: Local<PrepareNextFrameMaterials<M>>,
    mut extracted_assets: ResMut<ExtractedMaterials<M>>,
    mut render_materials: ResMut<RenderMaterials<M>>,
    render_device: Res<RenderDevice>,
    images: Res<RenderAssets<Image>>,
    fallback_image: Res<FallbackImage>,
    pipeline: Res<AnimatedMaterialPipeline<M>>,
) {
    let mut queued_assets = std::mem::take(&mut prepare_next_frame.assets);
    for (handle, material) in queued_assets.drain(..) {
        match prepare_material(
            &material,
            &render_device,
            &images,
            &fallback_image,
            &pipeline,
        ) {
            Ok(prepared_asset) => {
                render_materials.insert(handle, prepared_asset);
            }
            Err(AsBindGroupError::RetryNextUpdate) => {
                prepare_next_frame.assets.push((handle, material));
            }
        }
    }

    for removed in std::mem::take(&mut extracted_assets.removed) {
        render_materials.remove(&removed);
    }

    for (handle, material) in std::mem::take(&mut extracted_assets.extracted) {
        match prepare_material(
            &material,
            &render_device,
            &images,
            &fallback_image,
            &pipeline,
        ) {
            Ok(prepared_asset) => {
                render_materials.insert(handle, prepared_asset);
            }
            Err(AsBindGroupError::RetryNextUpdate) => {
                prepare_next_frame.assets.push((handle, material));
            }
        }
    }
}

fn prepare_material<M: Material>(
    material: &M,
    render_device: &RenderDevice,
    images: &RenderAssets<Image>,
    fallback_image: &FallbackImage,
    pipeline: &AnimatedMaterialPipeline<M>,
) -> Result<PreparedMaterial<M>, AsBindGroupError> {
    let prepared = material.as_bind_group(
        &pipeline.material_pipeline.material_layout,
        render_device,
        images,
        fallback_image,
    )?;
    Ok(PreparedMaterial {
        bindings: prepared.bindings,
        bind_group: prepared.bind_group,
        key: prepared.data,
        properties: MaterialProperties {
            alpha_mode: material.alpha_mode(),
            depth_bias: material.depth_bias(),
        },
    })
}

impl ExtractComponent for AnimationTime {
    type Query = Read<AnimationTime>;

    type Filter = ();

    fn extract_component(time: bevy::ecs::query::QueryItem<Self::Query>) -> Self {
        time.clone()
    }
}

#[allow(clippy::too_many_arguments)]
pub fn queue_time_animated_material_meshes<M: Material>(
    opaque_draw_functions: Res<DrawFunctions<Opaque3d>>,
    alpha_mask_draw_functions: Res<DrawFunctions<AlphaMask3d>>,
    transparent_draw_functions: Res<DrawFunctions<Transparent3d>>,
    material_pipeline: Res<AnimatedMaterialPipeline<M>>,
    mut pipelines: ResMut<SpecializedMeshPipelines<AnimatedMaterialPipeline<M>>>,
    mut pipeline_cache: ResMut<PipelineCache>,
    msaa: Res<Msaa>,
    render_meshes: Res<RenderAssets<Mesh>>,
    render_materials: Res<RenderMaterials<M>>,
    material_meshes: Query<(&Handle<M>, &Handle<Mesh>, &MeshUniform)>,
    mut views: Query<(
        &ExtractedView,
        &VisibleEntities,
        &mut RenderPhase<Opaque3d>,
        &mut RenderPhase<AlphaMask3d>,
        &mut RenderPhase<Transparent3d>,
    )>,
) where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    for (view, visible_entities, mut opaque_phase, mut alpha_mask_phase, mut transparent_phase) in
        &mut views
    {
        let draw_opaque_pbr = opaque_draw_functions
            .read()
            .get_id::<DrawAnimatedMaterial<M>>()
            .unwrap();
        let draw_alpha_mask_pbr = alpha_mask_draw_functions
            .read()
            .get_id::<DrawAnimatedMaterial<M>>()
            .unwrap();
        let draw_transparent_pbr = transparent_draw_functions
            .read()
            .get_id::<DrawAnimatedMaterial<M>>()
            .unwrap();

        let rangefinder = view.rangefinder3d();
        let msaa_key = MeshPipelineKey::from_msaa_samples(msaa.samples);

        for visible_entity in &visible_entities.entities {
            if let Ok((material_handle, mesh_handle, mesh_uniform)) =
                material_meshes.get(*visible_entity)
            {
                if let Some(material) = render_materials.get(material_handle) {
                    if let Some(mesh) = render_meshes.get(mesh_handle) {
                        let mut mesh_key =
                            MeshPipelineKey::from_primitive_topology(mesh.primitive_topology)
                                | msaa_key;
                        let alpha_mode = material.properties.alpha_mode;
                        if let AlphaMode::Blend = alpha_mode {
                            mesh_key |= MeshPipelineKey::TRANSPARENT_MAIN_PASS;
                        }

                        let pipeline_id = pipelines.specialize(
                            &mut pipeline_cache,
                            &material_pipeline,
                            MaterialPipelineKey {
                                mesh_key,
                                bind_group_data: material.key.clone(),
                            },
                            &mesh.layout,
                        );
                        let pipeline_id = match pipeline_id {
                            Ok(id) => id,
                            Err(err) => {
                                error!("{}", err);
                                continue;
                            }
                        };

                        let distance = rangefinder.distance(&mesh_uniform.transform)
                            + material.properties.depth_bias;
                        match alpha_mode {
                            AlphaMode::Opaque => {
                                opaque_phase.add(Opaque3d {
                                    entity: *visible_entity,
                                    draw_function: draw_opaque_pbr,
                                    pipeline: pipeline_id,
                                    distance,
                                });
                            }
                            AlphaMode::Mask(_) => {
                                alpha_mask_phase.add(AlphaMask3d {
                                    entity: *visible_entity,
                                    draw_function: draw_alpha_mask_pbr,
                                    pipeline: pipeline_id,
                                    distance,
                                });
                            }
                            AlphaMode::Blend => {
                                transparent_phase.add(Transparent3d {
                                    entity: *visible_entity,
                                    draw_function: draw_transparent_pbr,
                                    pipeline: pipeline_id,
                                    distance,
                                });
                            }
                        }
                    }
                }
            }
        }
    }
}

struct AnimationTimeBindGroup {
    bind_group: Option<BindGroup>,
}

// create a bind group for the time uniform buffer
fn queue_time_bind_group<M: Material>(
    render_device: Res<RenderDevice>,
    mut time_meta: ResMut<AnimationTimeBindGroup>,
    pipeline: Res<AnimatedMaterialPipeline<M>>,
    animation_time_uniforms: Res<ComponentUniforms<AnimationTime>>,
) {
    if let Some(binding) = animation_time_uniforms.uniforms().binding() {
        let bind_group = render_device.create_bind_group(&BindGroupDescriptor {
            label: Some("animation_time_bind_group"),
            layout: &pipeline.time_bind_group_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: binding,
            }],
        });
        time_meta.bind_group = Some(bind_group);
    }
}

pub struct AnimatedMaterialPipeline<M: Material> {
    material_pipeline: MaterialPipeline<M>,
    time_bind_group_layout: BindGroupLayout,
}

impl<M: Material> FromWorld for AnimatedMaterialPipeline<M> {
    fn from_world(world: &mut World) -> Self {
        let mut material_pipeline = MaterialPipeline::from_world(world);
        let asset_server = world.resource::<AssetServer>();
        // TODO: make it customizable somehow.
        material_pipeline.vertex_shader =
            Some(asset_server.load("shaders/time_animated_vertex_shader.wgsl"));
        material_pipeline.fragment_shader =
            Some(asset_server.load("shaders/time_animated_material_shader.wgsl"));

        let render_device = world.resource::<RenderDevice>();
        let time_bind_group_layout =
            render_device.create_bind_group_layout(&BindGroupLayoutDescriptor {
                label: Some("time bind group"),
                entries: &[BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: true,
                        min_binding_size: Some(AnimationTime::min_size()),
                    },
                    count: None,
                }],
            });

        AnimatedMaterialPipeline {
            material_pipeline,
            time_bind_group_layout,
        }
    }
}

impl<M: Material> SpecializedMeshPipeline for AnimatedMaterialPipeline<M>
where
    M::Data: PartialEq + Eq + Hash + Clone,
{
    type Key = MaterialPipelineKey<M>;

    fn specialize(
        &self,
        key: Self::Key,
        layout: &MeshVertexBufferLayout,
    ) -> Result<RenderPipelineDescriptor, SpecializedMeshPipelineError> {
        let mut descriptor = self.material_pipeline.specialize(key, layout)?;
        descriptor
            .layout
            .as_mut()
            .unwrap()
            .push(self.time_bind_group_layout.clone());
        Ok(descriptor)
    }
}

type DrawAnimatedMaterial<M> = (
    SetItemPipeline,
    SetMeshViewBindGroup<0>,
    SetMaterialBindGroup<M, 1>,
    SetMeshBindGroup<2>,
    SetTimeBindGroup<3>,
    DrawMesh,
);

struct SetTimeBindGroup<const I: usize>;

impl<const I: usize> EntityRenderCommand for SetTimeBindGroup<I> {
    type Param = (
        SRes<AnimationTimeBindGroup>,
        SQuery<Read<DynamicUniformIndex<AnimationTime>>>,
    );

    fn render<'w>(
        _view: Entity,
        item: Entity,
        (time_meta, query): SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let uniform_index = query.get(item).unwrap();
        let time_bind_group = time_meta.into_inner().bind_group.as_ref().unwrap();
        pass.set_bind_group(I, time_bind_group, &[uniform_index.index()]);

        RenderCommandResult::Success
    }
}
