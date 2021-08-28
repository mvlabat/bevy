use bevy::prelude::*;
use bevy::ecs::system::QueryableMut;

#[derive(Debug)]
struct ComponentA;
#[derive(Debug)]
struct ComponentB;
#[derive(Debug)]
struct ComponentC;
#[derive(Debug)]
struct ComponentD;

fn startup_system(mut commands: Commands) {
    commands.spawn()
        .insert(ComponentA)
        .insert(ComponentB)
        .insert(ComponentC);
    commands.spawn()
        .insert(ComponentA)
        .insert(ComponentB)
        .insert(ComponentD);
}

fn system_one(mut q: Query<(&ComponentA, &ComponentB, &ComponentC)>) {
    let mut v = q.map(|(a, b, _c)| (a, b));
    service(&mut v);
}

fn system_two(mut q: Query<(&ComponentA, &ComponentB, &ComponentD)>) {
    let mut v = q.map(|(a, b, _)| (a, b));
    service(&mut v);
}

fn service<'s>(q: &'s mut impl QueryableMut<'s, Item = (&'s ComponentA, &'s ComponentB)>) {
    for (a, b) in q.iter_mut() {
        println!("{:?} {:?}", a, b);
    }
}

fn main() {
    App::new()
        .add_startup_system(startup_system)
        .add_system(system_one)
        .add_system(system_two)
        .run();
}
