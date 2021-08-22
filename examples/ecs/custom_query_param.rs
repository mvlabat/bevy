use bevy::{
    ecs::{
        component::Component,
        query::{Fetch, FilterFetch},
    },
    prelude::*,
};
use std::{fmt::Debug, marker::PhantomData};

/// This examples illustrates the usage of `Fetch` and `FilterFetch` derive macros, that allow
/// defining custom query and filter types.
///
/// White regular tuple queries work great in most of simple scenarios, using custom queries
/// declared as named structs can bring the following advantages:
/// - They help to avoid destructuring or using `q.0, q.1, ...` access pattern.
/// - Adding, removing components or changing items order with structs greatly reduces maintenance
///   burden, as you don't need to update statements that destructure tuples, care abort order
///   of elements, etc. Instead, you can just add or remove places where a certain element is used.
/// - Named structs enable the composition pattern, that makes query types easier to re-use.
/// - You can bypass the limit of 15 components that exists for query tuples.
///
/// For more details on the `Fetch` and `FilterFetch` derive macros, see their documentation.
fn main() {
    AppBuilder::default()
        .add_startup_system(spawn.system())
        .add_system(print_components.system().label("print_components"))
        .add_system(
            print_components_readonly
                .system()
                .label("print_components_readonly")
                .after("print_components"),
        )
        .add_system(
            print_components_tuple
                .system()
                .after("print_components_readonly"),
        )
        .run();
}

#[derive(Debug)]
struct ComponentA;
#[derive(Debug)]
struct ComponentB;
#[derive(Debug)]
struct ComponentC;
#[derive(Debug)]
struct ComponentD;
#[derive(Debug)]
struct ComponentZ;

#[derive(Fetch)]
struct MutQuery<'w, T: Component + Debug, P: Component + Debug> {
    entity: Entity,
    // `Mut<'w, T>` is a necessary replacement for `&'w mut T`
    a: Mut<'w, ComponentA>,
    b: Option<Mut<'w, ComponentB>>,
    nested: NestedQuery<'w>,
    generic: GenericQuery<'w, T, P>,
}

// If you want to declare a read-only query that uses nested `Fetch` structs, you need to
// specify `readonly` attribute for the corresponding fields. This will generate static assertions
// that those members implement `ReadOnlyFetch`.
#[derive(Fetch)]
struct ReadOnlyNumQuery<'w, T: Component, P: Component> {
    entity: Entity,
    a: &'w ComponentA,
    b: &'w ComponentB,
    #[readonly]
    nested: NestedQuery<'w>,
    #[readonly]
    generic: GenericQuery<'w, T, P>,
}

#[derive(Fetch, Debug)]
struct NestedQuery<'w> {
    c: &'w ComponentC,
    d: Option<&'w ComponentD>,
}

#[derive(Fetch, Debug)]
struct GenericQuery<'w, T: Component, P: Component> {
    generic: (&'w T, &'w P),
}

#[derive(FilterFetch)]
struct QueryFilter<T: Component, P: Component> {
    _c: With<ComponentC>,
    _d: With<ComponentD>,
    _or: Or<(Added<ComponentC>, Changed<ComponentD>, Without<ComponentZ>)>,
    _generic_tuple: (With<T>, With<P>),
    _tp: PhantomData<(T, P)>,
}

fn spawn(mut commands: Commands) {
    commands
        .spawn()
        .insert(ComponentA)
        .insert(ComponentB)
        .insert(ComponentC)
        .insert(ComponentD);
}

fn print_components(
    mut query: Query<MutQuery<ComponentC, ComponentD>, QueryFilter<ComponentC, ComponentD>>,
) {
    println!("Print components:");
    for e in query.iter_mut() {
        println!("Entity: {:?}", e.entity);
        println!("A: {:?}", e.a);
        println!("B: {:?}", e.b);
        println!("Nested: {:?}", e.nested);
        println!("Generic: {:?}", e.generic);
    }
    println!();
}

fn print_components_readonly(
    query: Query<ReadOnlyNumQuery<ComponentC, ComponentD>, QueryFilter<ComponentC, ComponentD>>,
) {
    println!("Print components (read-only):");
    for e in query.iter() {
        println!("Entity: {:?}", e.entity);
        println!("A: {:?}", e.a);
        println!("B: {:?}", e.b);
        println!("Nested: {:?}", e.nested);
        println!("Generic: {:?}", e.generic);
    }
    println!();
}

type NestedTupleQuery<'w> = (&'w ComponentC, &'w ComponentD);
type GenericTupleQuery<'w, T, P> = (&'w T, &'w P);

fn print_components_tuple(
    query: Query<
        (
            Entity,
            &ComponentA,
            &ComponentB,
            NestedTupleQuery,
            GenericTupleQuery<ComponentC, ComponentD>,
        ),
        (
            With<ComponentC>,
            With<ComponentD>,
            Or<(Added<ComponentC>, Changed<ComponentD>, Without<ComponentZ>)>,
        ),
    >,
) {
    println!("Print components (tuple):");
    for (entity, a, b, nested, (generic_c, generic_d)) in query.iter() {
        println!("Entity: {:?}", entity);
        println!("A: {:?}", a);
        println!("B: {:?}", b);
        println!("Nested: {:?} {:?}", nested.0, nested.1);
        println!("Generic: {:?} {:?}", generic_c, generic_d);
    }
}
