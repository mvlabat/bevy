use bevy::ecs::component::Component;
use bevy::{
    ecs::query::{Fetch, FilterFetch},
    prelude::*,
};
use std::marker::PhantomData;

fn main() {
    App::new()
        .add_startup_system(spawn)
        .add_system(print_nums)
        .add_system(print_nums_readonly)
        .run();
}

#[derive(Fetch, Debug)]
struct NumQuery<'w, T: Component, P: Component> {
    entity: Entity,
    u: UNumQuery<'w>,
    i: INumQuery<'w>,
    generic: MutGenericQuery<'w, T, P>,
    #[filter(NumQueryFilter<T, P>)]
    filter: bool,
}

// If you want to declare a read-only query that uses nested `Fetch` structs, you need to
// specify `readonly` attribute for the corresponding fields. This will generate static assertions
// that those members implement `ReadOnlyFetch`.
#[derive(Fetch, Debug)]
struct ReadOnlyNumQuery<'w, T: Component, P: Component> {
    entity: Entity,
    #[readonly]
    u: UNumQuery<'w>,
    #[readonly]
    generic: GenericQuery<'w, T, P>,
    #[filter(NumQueryFilter<T, P>)]
    filter: bool,
}

#[derive(Fetch, Debug)]
struct UNumQuery<'w> {
    u_16: &'w u16,
    u_32_opt: Option<&'w u32>,
}

#[derive(Fetch, Debug)]
struct MutGenericQuery<'w, T: Component, P: Component> {
    generic: (Mut<'w, T>, Mut<'w, P>),
}

#[derive(Fetch, Debug)]
struct GenericQuery<'w, T: Component, P: Component> {
    generic: (&'w T, &'w P),
}

#[derive(Fetch, Debug)]
struct INumQuery<'w> {
    i_16: Mut<'w, i16>,
    i_32_opt: Option<Mut<'w, i32>>,
}

#[derive(FilterFetch)]
struct NumQueryFilter<T: Component, P: Component> {
    _u_16: With<u16>,
    _u_32: With<u32>,
    _or: Or<(With<i16>, Changed<u16>, Added<u32>)>,
    _generic_tuple: (With<T>, With<P>),
    _without: Without<Option<u16>>,
    _tp: PhantomData<(T, P)>,
}

fn spawn(mut commands: Commands) {
    commands
        .spawn()
        .insert(1u16)
        .insert(2u32)
        .insert(3i16)
        .insert(4i32)
        .insert(5u64)
        .insert(6i64);
}

fn print_nums(mut query: Query<NumQuery<u64, i64>, NumQueryFilter<u64, i64>>) {
    for num in query.iter_mut() {
        println!("Print: {:#?}", num);
    }
}

fn print_nums_readonly(query: Query<ReadOnlyNumQuery<u64, i64>, NumQueryFilter<u64, i64>>) {
    for num in query.iter() {
        println!("Print read-only: {:#?}", num);
    }
}
