use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Data, DataStruct, DeriveInput, Fields,
    GenericArgument, GenericParam, ImplGenerics, Lifetime, LifetimeDef, PathArguments, Token, Type,
    TypeGenerics, TypePath, TypeReference, WhereClause,
};

use crate::bevy_ecs_path;

static READONLY_ATTRIBUTE_NAME: &str = "readonly";
static FILTER_ATTRIBUTE_NAME: &str = "filter";

pub fn derive_fetch_impl(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let FetchImplTokens {
        struct_name,
        fetch_struct_name,
        state_struct_name,
        fetch_trait_punctuated_lifetimes,
        impl_generics,
        ty_generics,
        where_clause,
        struct_has_world_lifetime,
        world_lifetime,
        state_lifetime,
    } = fetch_impl_tokens(&ast);

    // Fetch's HRTBs require this hack to make the implementation compile. I don't fully understand
    // why this works though. If anyone's curious enough to try to find a better work-around, I'll
    // leave playground links here:
    // - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=da5e260a5c2f3e774142d60a199e854a (this fails)
    // - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=802517bb3d8f83c45ee8c0be360bb250 (this compiles)
    let mut fetch_generics = ast.generics.clone();
    fetch_generics.params.insert(0, state_lifetime);
    if !struct_has_world_lifetime {
        fetch_generics.params.insert(0, world_lifetime);
    }
    fetch_generics
        .params
        .push(GenericParam::Lifetime(LifetimeDef::new(Lifetime::new(
            "'fetch",
            Span::call_site(),
        ))));
    let (fetch_impl_generics, _, _) = fetch_generics.split_for_impl();
    let mut fetch_generics = ast.generics.clone();
    if struct_has_world_lifetime {
        *fetch_generics.params.first_mut().unwrap() =
            GenericParam::Lifetime(LifetimeDef::new(Lifetime::new("'fetch", Span::call_site())));
    }
    let (_, fetch_ty_generics, _) = fetch_generics.split_for_impl();

    let path = bevy_ecs_path();

    let fields = match &ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("Expected a struct with named fields"),
    };

    let mut is_readonly = true;

    let mut phantom_field_idents = Vec::new();
    let mut phantom_field_types = Vec::new();
    let mut field_idents = Vec::new();
    let mut filter_field_idents = Vec::new();
    let mut non_filter_field_idents = Vec::new();
    let mut query_types = Vec::new();
    let mut fetch_init_types = Vec::new();
    let mut readonly_types_to_assert = Vec::new();

    let generic_names = ast
        .generics
        .params
        .iter()
        .filter_map(|param| match param {
            GenericParam::Type(ty) => Some(ty.ident.to_string()),
            _ => None,
        })
        .collect::<Vec<_>>();

    for field in fields.iter() {
        let has_readonly_attribute = field.attrs.iter().any(|attr| {
            attr.path
                .get_ident()
                .map_or(false, |ident| ident == READONLY_ATTRIBUTE_NAME)
        });
        let filter_type = field
            .attrs
            .iter()
            .find(|attr| {
                attr.path
                    .get_ident()
                    .map_or(false, |ident| ident == FILTER_ATTRIBUTE_NAME)
            })
            .map(|filter| {
                filter
                    .parse_args::<Type>()
                    .expect("Expected a filter type (example: `#[filter(With<T>)]`)")
            });
        let is_filter = filter_type.is_some();

        let WorldQueryFieldTypeInfo {
            query_type,
            fetch_init_type: init_type,
            is_readonly: field_is_readonly,
            is_phantom,
            readonly_types_to_assert: field_readonly_types_to_assert,
        } = read_world_query_field_type_info(
            &field.ty,
            false,
            filter_type,
            has_readonly_attribute,
            &generic_names,
        );

        let field_ident = field.ident.as_ref().unwrap().clone();
        if is_phantom {
            phantom_field_idents.push(field_ident.clone());
            phantom_field_types.push(field.ty.clone());
        } else if is_filter {
            field_idents.push(field_ident.clone());
            filter_field_idents.push(field_ident.clone());
            query_types.push(query_type);
            fetch_init_types.push(init_type);
        } else {
            field_idents.push(field_ident.clone());
            non_filter_field_idents.push(field_ident.clone());
            query_types.push(query_type);
            fetch_init_types.push(init_type);
        }
        is_readonly = is_readonly && field_is_readonly;
        readonly_types_to_assert.extend(field_readonly_types_to_assert.into_iter());
    }

    let readonly_impl = if is_readonly {
        quote! {
            /// SAFETY: each item in the struct is read only
            unsafe impl #impl_generics #path::query::ReadOnlyFetch for #fetch_struct_name #ty_generics #where_clause {}

            // Statically checks that the safety guarantee actually holds true. We need this to make
            // sure that we don't compile `ReadOnlyFetch` if our struct contains nested `WorldQuery`
            // members that don't implement it.
            #[allow(dead_code)]
            const _: () = {
                fn assert_readonly<T: #path::query::ReadOnlyFetch>() {}

                // We generate a readonly assertion for every type that isn't &T, &mut T, Option<&T> or Option<&mut T>
                fn assert_all #impl_generics () #where_clause {
                    #(assert_readonly::<<#readonly_types_to_assert as #path::query::WorldQuery>::Fetch>();)*
                }
            };
        }
    } else {
        quote! {}
    };

    let tokens = TokenStream::from(quote! {
        struct #fetch_struct_name #impl_generics #where_clause {
            #(#field_idents: <#query_types as #path::query::WorldQuery>::Fetch,)*
        }

        struct #state_struct_name #impl_generics #where_clause {
            #(#field_idents: <#query_types as #path::query::WorldQuery>::State,)*
        }

        impl #fetch_impl_generics #path::query::Fetch<#fetch_trait_punctuated_lifetimes> for #fetch_struct_name #fetch_ty_generics #where_clause {
            type Item = #struct_name #ty_generics;
            type State = #state_struct_name #fetch_ty_generics;

            unsafe fn init(_world: &#path::world::World, state: &Self::State, _last_change_tick: u32, _change_tick: u32) -> Self {
                #fetch_struct_name {
                    #(#field_idents: <#fetch_init_types as #path::query::WorldQuery>::Fetch::init(_world, &state.#field_idents, _last_change_tick, _change_tick),)*
                }
            }

            #[inline]
            fn is_dense(&self) -> bool {
                true #(&& self.#field_idents.is_dense())*
            }

            /// SAFETY: we call `set_archetype` for each member that implements `Fetch`
            #[inline]
            unsafe fn set_archetype(&mut self, _state: &Self::State, _archetype: &#path::archetype::Archetype, _tables: &#path::storage::Tables) {
                #(self.#field_idents.set_archetype(&_state.#field_idents, _archetype, _tables);)*
            }

            /// SAFETY: we call `set_table` for each member that implements `Fetch`
            #[inline]
            unsafe fn set_table(&mut self, _state: &Self::State, _table: &#path::storage::Table) {
                #(self.#field_idents.set_table(&_state.#field_idents, _table);)*
            }

            /// SAFETY: we call `table_fetch` for each member that implements `Fetch` or
            /// `table_filter_fetch` if it also implements `FilterFetch`.
            #[inline]
            unsafe fn table_fetch(&mut self, _table_row: usize) -> Self::Item {
                use #path::query::FilterFetch;
                #struct_name {
                    #(#non_filter_field_idents: self.#non_filter_field_idents.table_fetch(_table_row),)*
                    #(#filter_field_idents: self.#filter_field_idents.table_filter_fetch(_table_row),)*
                    #(#phantom_field_idents: Default::default(),)*
                }
            }

            /// SAFETY: we call `archetype_fetch` for each member that implements `Fetch` or
            /// `archetype_filter_fetch` if it also implements `FilterFetch`.
            #[inline]
            unsafe fn archetype_fetch(&mut self, _archetype_index: usize) -> Self::Item {
                use #path::query::FilterFetch;
                #struct_name {
                    #(#non_filter_field_idents: self.#non_filter_field_idents.archetype_fetch(_archetype_index),)*
                    #(#filter_field_idents: self.#filter_field_idents.archetype_filter_fetch(_archetype_index),)*
                    #(#phantom_field_idents: Default::default(),)*
                }
            }
        }

        // SAFETY: `update_component_access` and `update_archetype_component_access` are called for each item in the struct
        unsafe impl #impl_generics #path::query::FetchState for #state_struct_name #ty_generics #where_clause {
            fn init(world: &mut #path::world::World) -> Self {
                #state_struct_name {
                    #(#field_idents: <#query_types as #path::query::WorldQuery>::State::init(world),)*
                }
            }

            fn update_component_access(&self, _access: &mut #path::query::FilteredAccess<#path::component::ComponentId>) {
                #(self.#field_idents.update_component_access(_access);)*
            }

            fn update_archetype_component_access(&self, _archetype: &#path::archetype::Archetype, _access: &mut #path::query::Access<#path::archetype::ArchetypeComponentId>) {
                #(self.#field_idents.update_archetype_component_access(_archetype, _access);)*
            }

            fn matches_archetype(&self, _archetype: &#path::archetype::Archetype) -> bool {
                true #(&& self.#field_idents.matches_archetype(_archetype))*
            }

            fn matches_table(&self, _table: &#path::storage::Table) -> bool {
                true #(&& self.#field_idents.matches_table(_table))*
            }
        }

        impl #impl_generics #path::query::WorldQuery for #struct_name #ty_generics #where_clause {
            type Fetch = #fetch_struct_name #ty_generics;
            type State = #state_struct_name #ty_generics;
        }

        #readonly_impl
    });
    tokens
}

pub fn derive_filter_fetch_impl(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let FetchImplTokens {
        struct_name,
        fetch_struct_name,
        state_struct_name,
        fetch_trait_punctuated_lifetimes,
        impl_generics,
        ty_generics,
        where_clause,
        struct_has_world_lifetime,
        world_lifetime,
        state_lifetime,
    } = fetch_impl_tokens(&ast);

    // Fetch's HRTBs require this hack to make the implementation compile. I don't fully understand
    // why this works though. If anyone's curious enough to try to find a better work-around, I'll
    // leave playground links here:
    // - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=da5e260a5c2f3e774142d60a199e854a (this fails)
    // - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=802517bb3d8f83c45ee8c0be360bb250 (this compiles)
    let mut fetch_generics = ast.generics.clone();
    fetch_generics.params.insert(0, state_lifetime);
    if !struct_has_world_lifetime {
        fetch_generics.params.insert(0, world_lifetime);
    }
    fetch_generics
        .params
        .push(GenericParam::Lifetime(LifetimeDef::new(Lifetime::new(
            "'fetch",
            Span::call_site(),
        ))));
    let (fetch_impl_generics, _, _) = fetch_generics.split_for_impl();
    let mut fetch_generics = ast.generics.clone();
    if struct_has_world_lifetime {
        *fetch_generics.params.first_mut().unwrap() =
            GenericParam::Lifetime(LifetimeDef::new(Lifetime::new("'fetch", Span::call_site())));
    }
    let (_, fetch_ty_generics, _) = fetch_generics.split_for_impl();

    let path = bevy_ecs_path();

    let fields = match &ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("Expected a struct with named fields"),
    };

    let mut phantom_field_idents = Vec::new();
    let mut phantom_field_types = Vec::new();
    let mut field_idents = Vec::new();
    let mut field_types = Vec::new();

    for field in fields.iter() {
        let is_phantom = match &field.ty {
            Type::Path(ty_path) => {
                let last_segment = ty_path.path.segments.last().unwrap();
                last_segment.ident == "PhantomData"
            }
            _ => false,
        };

        let field_ident = field.ident.as_ref().unwrap().clone();
        if is_phantom {
            phantom_field_idents.push(field_ident.clone());
            phantom_field_types.push(field.ty.clone());
        } else {
            field_idents.push(field_ident.clone());
            field_types.push(field.ty.clone());
        }
    }

    let tokens = TokenStream::from(quote! {
        struct #fetch_struct_name #impl_generics #where_clause {
            #(#field_idents: <#field_types as #path::query::WorldQuery>::Fetch,)*
            #(#phantom_field_idents: #phantom_field_types,)*
        }

        struct #state_struct_name #impl_generics #where_clause {
            #(#field_idents: <#field_types as #path::query::WorldQuery>::State,)*
            #(#phantom_field_idents: #phantom_field_types,)*
        }

        impl #fetch_impl_generics #path::query::Fetch<#fetch_trait_punctuated_lifetimes> for #fetch_struct_name #fetch_ty_generics #where_clause {
            type Item = bool;
            type State = #state_struct_name #fetch_ty_generics;

            unsafe fn init(_world: &#path::world::World, state: &Self::State, _last_change_tick: u32, _change_tick: u32) -> Self {
                #fetch_struct_name {
                    #(#field_idents: <#field_types as #path::query::WorldQuery>::Fetch::init(_world, &state.#field_idents, _last_change_tick, _change_tick),)*
                    #(#phantom_field_idents: Default::default(),)*
                }
            }

            #[inline]
            fn is_dense(&self) -> bool {
                true #(&& self.#field_idents.is_dense())*
            }

            #[inline]
            unsafe fn set_archetype(&mut self, _state: &Self::State, _archetype: &#path::archetype::Archetype, _tables: &#path::storage::Tables) {
                #(self.#field_idents.set_archetype(&_state.#field_idents, _archetype, _tables);)*
            }

            #[inline]
            unsafe fn set_table(&mut self, _state: &Self::State, _table: &#path::storage::Table) {
                #(self.#field_idents.set_table(&_state.#field_idents, _table);)*
            }

            #[inline]
            unsafe fn table_fetch(&mut self, _table_row: usize) -> Self::Item {
                use #path::query::FilterFetch;
                true #(&& self.#field_idents.table_filter_fetch(_table_row))*
            }

            #[inline]
            unsafe fn archetype_fetch(&mut self, _archetype_index: usize) -> Self::Item {
                use #path::query::FilterFetch;
                true #(&& self.#field_idents.archetype_filter_fetch(_archetype_index))*
            }
        }

        // SAFETY: update_component_access and update_archetype_component_access are called for each item in the struct
        unsafe impl #impl_generics #path::query::FetchState for #state_struct_name #ty_generics #where_clause {
            fn init(world: &mut #path::world::World) -> Self {
                #state_struct_name {
                    #(#field_idents: <#field_types as #path::query::WorldQuery>::State::init(world),)*
                    #(#phantom_field_idents: Default::default(),)*
                }
            }

            fn update_component_access(&self, _access: &mut #path::query::FilteredAccess<#path::component::ComponentId>) {
                #(self.#field_idents.update_component_access(_access);)*
            }

            fn update_archetype_component_access(&self, _archetype: &#path::archetype::Archetype, _access: &mut #path::query::Access<#path::archetype::ArchetypeComponentId>) {
                #(self.#field_idents.update_archetype_component_access(_archetype, _access);)*
            }

            fn matches_archetype(&self, _archetype: &#path::archetype::Archetype) -> bool {
                true #(&& self.#field_idents.matches_archetype(_archetype))*
            }

            fn matches_table(&self, _table: &#path::storage::Table) -> bool {
                true #(&& self.#field_idents.matches_table(_table))*
            }
        }

        impl #impl_generics #path::query::WorldQuery for #struct_name #ty_generics #where_clause {
            type Fetch = #fetch_struct_name #ty_generics;
            type State = #state_struct_name #ty_generics;
        }

        /// SAFETY: each item in the struct is a fetch filter and thus is read only
        unsafe impl #impl_generics #path::query::ReadOnlyFetch for #fetch_struct_name #ty_generics #where_clause {}
    });
    tokens
}

// This struct is used to share common tokens between `Fetch` and `FilterFetch` implementations.
struct FetchImplTokens<'a> {
    struct_name: Ident,
    fetch_struct_name: Ident,
    state_struct_name: Ident,
    fetch_trait_punctuated_lifetimes: Punctuated<GenericParam, Token![,]>,
    impl_generics: ImplGenerics<'a>,
    ty_generics: TypeGenerics<'a>,
    where_clause: Option<&'a WhereClause>,
    struct_has_world_lifetime: bool,
    world_lifetime: GenericParam,
    state_lifetime: GenericParam,
}

fn fetch_impl_tokens(ast: &DeriveInput) -> FetchImplTokens {
    let world_lifetime = ast.generics.params.first().and_then(|param| match param {
        lt @ GenericParam::Lifetime(_) => Some(lt.clone()),
        _ => None,
    });
    let struct_has_world_lifetime = world_lifetime.is_some();
    let world_lifetime = world_lifetime.unwrap_or_else(|| {
        GenericParam::Lifetime(LifetimeDef::new(Lifetime::new("'world", Span::call_site())))
    });
    let state_lifetime =
        GenericParam::Lifetime(LifetimeDef::new(Lifetime::new("'state", Span::call_site())));

    let mut fetch_trait_punctuated_lifetimes = Punctuated::<_, Token![,]>::new();
    fetch_trait_punctuated_lifetimes.push(world_lifetime.clone());
    fetch_trait_punctuated_lifetimes.push(state_lifetime.clone());

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let struct_name = ast.ident.clone();
    let fetch_struct_name = Ident::new(&format!("{}Fetch", struct_name), Span::call_site());
    let state_struct_name = Ident::new(&format!("{}State", struct_name), Span::call_site());

    FetchImplTokens {
        struct_name,
        fetch_struct_name,
        state_struct_name,
        fetch_trait_punctuated_lifetimes,
        impl_generics,
        ty_generics,
        where_clause,
        struct_has_world_lifetime,
        world_lifetime,
        state_lifetime,
    }
}

struct WorldQueryFieldTypeInfo {
    /// We convert `Mut<T>` to `&mut T` (because this is the type that implements `WorldQuery`)
    /// and store it here.
    query_type: Type,
    /// The same as `query_type` but with `'fetch` lifetime.
    fetch_init_type: Type,
    is_readonly: bool,
    is_phantom: bool,
    readonly_types_to_assert: Vec<TypePath>,
}

fn read_world_query_field_type_info(
    ty: &Type,
    is_tuple_element: bool,
    filter_type: Option<Type>,
    has_readonly_attribute: bool,
    generic_names: &[String],
) -> WorldQueryFieldTypeInfo {
    let mut query_type = ty.clone();
    let mut fetch_init_type = ty.clone();
    let mut is_readonly = true;
    let mut is_phantom = false;
    let mut readonly_types_to_assert = Vec::new();

    match (ty, &mut fetch_init_type) {
        (Type::Path(path), Type::Path(path_init)) => {
            if path.qself.is_some() {
                // There's a risk that it contains a generic parameter that we can't test
                // whether it's readonly or not.
                panic!("Self type qualifiers aren't supported");
            }

            let segment = path.path.segments.last().unwrap();
            if segment.ident == "Option" {
                // We expect that `Option` stores either `&T` or `Mut<T>`.
                let ty = match &segment.arguments {
                    PathArguments::AngleBracketed(args) => {
                        args.args.last().and_then(|arg| match arg {
                            GenericArgument::Type(ty) => Some(ty),
                            _ => None,
                        })
                    }
                    _ => None,
                };
                match ty.expect("Option type is expected to have generic arguments") {
                    // If it's a read-only reference, we just update the lifetime for `fetch_init_type` to `'fetch`.
                    Type::Reference(reference) => {
                        if reference.mutability.is_some() {
                            panic!("Invalid reference type: use `Mut<T>` instead of `&mut T`");
                        }
                        match &mut path_init.path.segments.last_mut().unwrap().arguments {
                            PathArguments::AngleBracketed(args) => {
                                match args.args.last_mut().unwrap() {
                                    GenericArgument::Type(Type::Reference(ty)) => ty.lifetime = Some(Lifetime::new("'fetch", Span::call_site())),
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    // If it's a mutable reference, we set `query_type` and `fetch_init_type` to `&mut T`,
                    // we also update the lifetime for `fetch_init_type` to `'fetch`.
                    Type::Path(path) => {
                        assert_not_generic(path, generic_names);

                        let segment = path.path.segments.last().unwrap();
                        let ty_ident = &segment.ident;
                        if ty_ident == "Mut" {
                            is_readonly = false;
                            let (mut_lifetime, mut_ty) = match &segment.arguments {
                                PathArguments::AngleBracketed(args) => {
                                    (args.args.first().and_then(|arg| { match arg {
                                        GenericArgument::Lifetime(lifetime) => Some(lifetime.clone()),
                                        _ => None,
                                    }}).expect("Mut is expected to have a lifetime"),
                                     args.args.last().and_then(|arg| { match arg {
                                         GenericArgument::Type(ty) => Some(ty.clone()),
                                         _ => None,
                                     }}).expect("Mut is expected to have a lifetime"))
                                }
                                _ => panic!("Mut type is expected to have generic arguments")
                            };

                            match query_type {
                                Type::Path(ref mut path) => {
                                    let segment = path.path.segments.last_mut().unwrap();
                                    match segment.arguments {
                                        PathArguments::AngleBracketed(ref mut args) => {
                                            match args.args.last_mut().unwrap() {
                                                GenericArgument::Type(ty) => {
                                                    *ty = Type::Reference(TypeReference {
                                                        and_token: Token![&](Span::call_site()),
                                                        lifetime: Some(mut_lifetime),
                                                        mutability: Some(Token![mut](Span::call_site())),
                                                        elem: Box::new(mut_ty.clone()),
                                                    });
                                                }
                                                _ => unreachable!()
                                            }
                                        }
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }

                            let segment = path_init.path.segments.last_mut().unwrap();
                            match segment.arguments {
                                PathArguments::AngleBracketed(ref mut args) => {
                                    match args.args.last_mut().unwrap() {
                                        GenericArgument::Type(ty) => {
                                            *ty = Type::Reference(TypeReference {
                                                and_token: Token![&](Span::call_site()),
                                                lifetime: Some(Lifetime::new("'fetch", Span::call_site())),
                                                mutability: Some(Token![mut](Span::call_site())),
                                                elem: Box::new(mut_ty),
                                            });
                                        }
                                        _ => unreachable!()
                                    }
                                }
                                _ => unreachable!()
                            }
                        } else {
                            panic!("Option type is expected to have a reference value (`Option<&T>` or `Option<Mut<T>>`)");
                        }
                    }
                    _ => panic!("Option type is expected to have a reference value (`Option<&T>` or `Option<Mut<T>>`)"),
                }
            } else if segment.ident == "Mut" {
                is_readonly = false;
                // If it's a mutable reference, we set `query_type` and `fetch_init_type` to `&mut T`,
                // we also update the lifetime for `fetch_init_type` to `'fetch`.
                let (mut_lifetime, mut_ty) = match &segment.arguments {
                    PathArguments::AngleBracketed(args) => {
                        let lt = args.args.first().and_then(|arg| { match arg {
                            GenericArgument::Lifetime(lifetime) => Some(lifetime.clone()),
                            _ => None,
                        }}).expect("`Mut` is expected to have a lifetime");
                        let ty = args.args.last().and_then(|arg| { match arg {
                            GenericArgument::Type(ty) => Some(ty.clone()),
                            _ => None,
                        }}).expect("`Mut` is expected to have a lifetime");
                        (lt, ty)
                    }
                    _ => panic!("`Mut` is expected to have generic arguments")
                };

                query_type = Type::Reference(TypeReference {
                    and_token: Token![&](Span::call_site()),
                    lifetime: Some(mut_lifetime),
                    mutability: Some(Token![mut](Span::call_site())),
                    elem: Box::new(mut_ty.clone()),
                });
                fetch_init_type = Type::Reference(TypeReference {
                    and_token: Token![&](Span::call_site()),
                    lifetime: Some(Lifetime::new("'fetch", Span::call_site())),
                    mutability: Some(Token![mut](Span::call_site())),
                    elem: Box::new(mut_ty),
                });
            } else if segment.ident == "bool" {
                if is_tuple_element {
                    panic!("Invalid tuple element: bool");
                }
                fetch_init_type = filter_type.expect("Field type is `bool` but no `filter` attribute is found (example: `#[filter(With<T>)]`)");
                query_type = fetch_init_type.clone();
            } else if segment.ident == "With" || segment.ident == "Without" || segment.ident == "Or" || segment.ident == "Added" || segment.ident == "Changed" {
                panic!("Invalid filter type: use `bool` field type and specify the filter with `#[filter({}<T>)]` attribute", segment.ident.to_string());
            } else if segment.ident == "PhantomData" {
                if is_tuple_element {
                    panic!("Invalid tuple element: PhantomData");
                }
                is_phantom = true;
            } else if segment.ident != "Entity" {
                assert_not_generic(path, generic_names);

                if let PathArguments::AngleBracketed(args) = &mut path_init.path.segments.last_mut().unwrap().arguments {
                    if let Some(GenericArgument::Lifetime(lt)) = args.args.first_mut() {
                        *lt = Lifetime::new("'fetch", Span::call_site());
                    }
                }

                // If there's no `filter` attribute, we assume that it's a nested struct that implements `Fetch`.
                if filter_type.is_none() {
                    // If a user marks the field with the `readonly` attribute, we'll insert
                    // a function call (no calls will happen in runtime), that will check that
                    // the type implements `ReadOnlyFetch` indeed.
                    // We can't allow ourselves to implement `ReadOnlyFetch` for the current struct
                    // if we are not sure that all members implement it.
                    if has_readonly_attribute {
                        readonly_types_to_assert.push(path.clone());
                    } else {
                        is_readonly = false;
                    }
                }
            }
        }
        (Type::Reference(reference), Type::Reference(init_reference)) => {
            if reference.mutability.is_some() {
                panic!("Invalid reference type: use `Mut<T>` instead of `&mut T`");
            }
            init_reference.lifetime = Some(Lifetime::new("'fetch", Span::call_site()));
        }
        (Type::Tuple(tuple), Type::Tuple(init_tuple)) => {
            let mut query_tuple_elems = tuple.elems.clone();
            query_tuple_elems.clear();
            let mut fetch_init_tuple_elems = query_tuple_elems.clone();
            for ty in tuple.elems.iter() {
                let WorldQueryFieldTypeInfo {
                    query_type,
                    fetch_init_type,
                    is_readonly: elem_is_readonly,
                    is_phantom: _,
                    readonly_types_to_assert: elem_readonly_types_to_assert,
                } = read_world_query_field_type_info(
                    ty,
                    true,
                    None,
                    has_readonly_attribute,
                    generic_names,
                );
                query_tuple_elems.push(query_type);
                fetch_init_tuple_elems.push(fetch_init_type);
                is_readonly = is_readonly && elem_is_readonly;
                readonly_types_to_assert.extend(elem_readonly_types_to_assert.into_iter());
            }
            match query_type {
                Type::Tuple(ref mut tuple) => {
                    tuple.elems = query_tuple_elems;
                }
                _ => unreachable!(),
            }
            init_tuple.elems = fetch_init_tuple_elems;
        }
        _ => panic!("Only the following types (or their tuples) are supported for WorldQuery: &T, &mut T, Option<&T>, Option<&mut T>, Entity, or other structs that implement WorldQuery"),
    }

    WorldQueryFieldTypeInfo {
        query_type,
        fetch_init_type,
        is_readonly,
        is_phantom,
        readonly_types_to_assert,
    }
}

fn assert_not_generic(type_path: &TypePath, generic_names: &[String]) {
    // `get_ident` returns Some if it consists of a single segment, in this case it
    // makes sense to ensure that it's not a generic.
    if let Some(ident) = type_path.path.get_ident() {
        let is_generic = generic_names
            .iter()
            .any(|generic_name| ident == generic_name.as_str());
        if is_generic {
            panic!("Only references to generic types are supported: i.e. instead of `component: T`, use `component: &T` or `component: Mut<T>` (optional references are supported as well)");
        }
    }
}
