extern crate proc_macro;

use bevy_macro_utils::BevyManifest;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token::Comma,
    Data, DataStruct, DeriveInput, Field, Fields, GenericArgument, GenericParam, Ident,
    ImplGenerics, Index, Lifetime, LifetimeDef, LitInt, Path, PathArguments, Result, Token, Type,
    TypeGenerics, TypePath, TypeReference, WhereClause,
};

struct AllTuples {
    macro_ident: Ident,
    start: usize,
    end: usize,
    idents: Vec<Ident>,
}

impl Parse for AllTuples {
    fn parse(input: ParseStream) -> Result<Self> {
        let macro_ident = input.parse::<Ident>()?;
        input.parse::<Comma>()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Comma>()?;
        let end = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Comma>()?;
        let mut idents = vec![input.parse::<Ident>()?];
        while input.parse::<Comma>().is_ok() {
            idents.push(input.parse::<Ident>()?);
        }

        Ok(AllTuples {
            macro_ident,
            start,
            end,
            idents,
        })
    }
}

#[proc_macro]
pub fn all_tuples(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as AllTuples);
    let len = (input.start..=input.end).count();
    let mut ident_tuples = Vec::with_capacity(len);
    for i in input.start..=input.end {
        let idents = input
            .idents
            .iter()
            .map(|ident| format_ident!("{}{}", ident, i));
        if input.idents.len() < 2 {
            ident_tuples.push(quote! {
                #(#idents)*
            });
        } else {
            ident_tuples.push(quote! {
                (#(#idents),*)
            });
        }
    }

    let macro_ident = &input.macro_ident;
    let invocations = (input.start..=input.end).map(|i| {
        let ident_tuples = &ident_tuples[0..i];
        quote! {
            #macro_ident!(#(#ident_tuples),*);
        }
    });
    TokenStream::from(quote! {
        #(
            #invocations
        )*
    })
}

static BUNDLE_ATTRIBUTE_NAME: &str = "bundle";

#[proc_macro_derive(Bundle, attributes(bundle))]
pub fn derive_bundle(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ecs_path = bevy_ecs_path();

    let named_fields = match &ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("Expected a struct with named fields."),
    };

    let is_bundle = named_fields
        .iter()
        .map(|field| {
            field
                .attrs
                .iter()
                .any(|a| *a.path.get_ident().as_ref().unwrap() == BUNDLE_ATTRIBUTE_NAME)
        })
        .collect::<Vec<bool>>();
    let field = named_fields
        .iter()
        .map(|field| field.ident.as_ref().unwrap())
        .collect::<Vec<_>>();
    let field_type = named_fields
        .iter()
        .map(|field| &field.ty)
        .collect::<Vec<_>>();

    let mut field_component_ids = Vec::new();
    let mut field_get_components = Vec::new();
    let mut field_from_components = Vec::new();
    for ((field_type, is_bundle), field) in
        field_type.iter().zip(is_bundle.iter()).zip(field.iter())
    {
        if *is_bundle {
            field_component_ids.push(quote! {
                component_ids.extend(<#field_type as #ecs_path::bundle::Bundle>::component_ids(components));
            });
            field_get_components.push(quote! {
                self.#field.get_components(&mut func);
            });
            field_from_components.push(quote! {
                #field: <#field_type as #ecs_path::bundle::Bundle>::from_components(&mut func),
            });
        } else {
            field_component_ids.push(quote! {
                component_ids.push(components.get_or_insert_id::<#field_type>());
            });
            field_get_components.push(quote! {
                func((&mut self.#field as *mut #field_type).cast::<u8>());
                std::mem::forget(self.#field);
            });
            field_from_components.push(quote! {
                #field: func().cast::<#field_type>().read(),
            });
        }
    }
    let field_len = field.len();
    let generics = ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let struct_name = &ast.ident;

    TokenStream::from(quote! {
        /// SAFE: TypeInfo is returned in field-definition-order. [from_components] and [get_components] use field-definition-order
        unsafe impl #impl_generics #ecs_path::bundle::Bundle for #struct_name#ty_generics #where_clause {
            fn component_ids(
                components: &mut #ecs_path::component::Components,
            ) -> Vec<#ecs_path::component::ComponentId> {
                let mut component_ids = Vec::with_capacity(#field_len);
                #(#field_component_ids)*
                component_ids
            }

            #[allow(unused_variables, unused_mut, non_snake_case)]
            unsafe fn from_components(mut func: impl FnMut() -> *mut u8) -> Self {
                Self {
                    #(#field_from_components)*
                }
            }

            #[allow(unused_variables, unused_mut, forget_copy, forget_ref)]
            fn get_components(mut self, mut func: impl FnMut(*mut u8)) {
                #(#field_get_components)*
            }
        }
    })
}

fn get_idents(fmt_string: fn(usize) -> String, count: usize) -> Vec<Ident> {
    (0..count)
        .map(|i| Ident::new(&fmt_string(i), Span::call_site()))
        .collect::<Vec<Ident>>()
}

#[proc_macro]
pub fn impl_query_set(_input: TokenStream) -> TokenStream {
    let mut tokens = TokenStream::new();
    let max_queries = 4;
    let queries = get_idents(|i| format!("Q{}", i), max_queries);
    let filters = get_idents(|i| format!("F{}", i), max_queries);
    let mut query_fn_muts = Vec::new();
    for i in 0..max_queries {
        let query = &queries[i];
        let filter = &filters[i];
        let fn_name = Ident::new(&format!("q{}", i), Span::call_site());
        let index = Index::from(i);
        query_fn_muts.push(quote! {
            pub fn #fn_name(&mut self) -> Query<'_, '_, #query, #filter> {
                // SAFE: systems run without conflicts with other systems.
                // Conflicting queries in QuerySet are not accessible at the same time
                // QuerySets are guaranteed to not conflict with other SystemParams
                unsafe {
                    Query::new(self.world, &self.query_states.#index, self.last_change_tick, self.change_tick)
                }
            }
        });
    }

    for query_count in 1..=max_queries {
        let query = &queries[0..query_count];
        let filter = &filters[0..query_count];
        let query_fn_mut = &query_fn_muts[0..query_count];
        tokens.extend(TokenStream::from(quote! {
            impl<'w, 's, #(#query: WorldQuery + 'static,)* #(#filter: WorldQuery + 'static,)*> SystemParam for QuerySet<'w, 's, (#(QueryState<#query, #filter>,)*)>
                where #(#filter::Fetch: FilterFetch,)*
            {
                type Fetch = QuerySetState<(#(QueryState<#query, #filter>,)*)>;
            }

            // SAFE: All Queries are constrained to ReadOnlyFetch, so World is only read
            unsafe impl<#(#query: WorldQuery + 'static,)* #(#filter: WorldQuery + 'static,)*> ReadOnlySystemParamFetch for QuerySetState<(#(QueryState<#query, #filter>,)*)>
            where #(#query::Fetch: ReadOnlyFetch,)* #(#filter::Fetch: FilterFetch,)*
            { }

            // SAFE: Relevant query ComponentId and ArchetypeComponentId access is applied to SystemMeta. If any QueryState conflicts
            // with any prior access, a panic will occur.
            unsafe impl<#(#query: WorldQuery + 'static,)* #(#filter: WorldQuery + 'static,)*> SystemParamState for QuerySetState<(#(QueryState<#query, #filter>,)*)>
                where #(#filter::Fetch: FilterFetch,)*
            {
                type Config = ();
                fn init(world: &mut World, system_meta: &mut SystemMeta, config: Self::Config) -> Self {
                    #(
                        let mut #query = QueryState::<#query, #filter>::new(world);
                        assert_component_access_compatibility(
                            &system_meta.name,
                            std::any::type_name::<#query>(),
                            std::any::type_name::<#filter>(),
                            &system_meta.component_access_set,
                            &#query.component_access,
                            world,
                        );
                    )*
                    #(
                        system_meta
                            .component_access_set
                            .add(#query.component_access.clone());
                        system_meta
                            .archetype_component_access
                            .extend(&#query.archetype_component_access);
                    )*
                    QuerySetState((#(#query,)*))
                }

                fn new_archetype(&mut self, archetype: &Archetype, system_meta: &mut SystemMeta) {
                    let (#(#query,)*) = &mut self.0;
                    #(
                        #query.new_archetype(archetype);
                        system_meta
                            .archetype_component_access
                            .extend(&#query.archetype_component_access);
                    )*
                }

                fn default_config() {}
            }

            impl<'w, 's, #(#query: WorldQuery + 'static,)* #(#filter: WorldQuery + 'static,)*> SystemParamFetch<'w, 's> for QuerySetState<(#(QueryState<#query, #filter>,)*)>
                where #(#filter::Fetch: FilterFetch,)*
            {
                type Item = QuerySet<'w, 's, (#(QueryState<#query, #filter>,)*)>;

                #[inline]
                unsafe fn get_param(
                    state: &'s mut Self,
                    system_meta: &SystemMeta,
                    world: &'w World,
                    change_tick: u32,
                ) -> Self::Item {
                    QuerySet {
                        query_states: &state.0,
                        world,
                        last_change_tick: system_meta.last_change_tick,
                        change_tick,
                    }
                }
            }

            impl<'w, 's, #(#query: WorldQuery,)* #(#filter: WorldQuery,)*> QuerySet<'w, 's, (#(QueryState<#query, #filter>,)*)>
                where #(#filter::Fetch: FilterFetch,)*
            {
                #(#query_fn_mut)*
            }
        }));
    }

    tokens
}

#[derive(Default)]
struct SystemParamFieldAttributes {
    pub ignore: bool,
}

static SYSTEM_PARAM_ATTRIBUTE_NAME: &str = "system_param";

/// Implement `SystemParam` to use a struct as a parameter in a system
#[proc_macro_derive(SystemParam, attributes(system_param))]
pub fn derive_system_param(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let fields = match &ast.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("Expected a struct with named fields."),
    };
    let path = bevy_ecs_path();

    let field_attributes = fields
        .iter()
        .map(|field| {
            (
                field,
                field
                    .attrs
                    .iter()
                    .find(|a| *a.path.get_ident().as_ref().unwrap() == SYSTEM_PARAM_ATTRIBUTE_NAME)
                    .map_or_else(SystemParamFieldAttributes::default, |a| {
                        syn::custom_keyword!(ignore);
                        let mut attributes = SystemParamFieldAttributes::default();
                        a.parse_args_with(|input: ParseStream| {
                            if input.parse::<Option<ignore>>()?.is_some() {
                                attributes.ignore = true;
                            }
                            Ok(())
                        })
                        .expect("Invalid 'render_resources' attribute format.");

                        attributes
                    }),
            )
        })
        .collect::<Vec<(&Field, SystemParamFieldAttributes)>>();
    let mut fields = Vec::new();
    let mut field_indices = Vec::new();
    let mut field_types = Vec::new();
    let mut ignored_fields = Vec::new();
    let mut ignored_field_types = Vec::new();
    for (i, (field, attrs)) in field_attributes.iter().enumerate() {
        if attrs.ignore {
            ignored_fields.push(field.ident.as_ref().unwrap());
            ignored_field_types.push(&field.ty);
        } else {
            fields.push(field.ident.as_ref().unwrap());
            field_types.push(&field.ty);
            field_indices.push(Index::from(i));
        }
    }

    let generics = ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let lifetimeless_generics: Vec<_> = generics
        .params
        .iter()
        .filter(|g| matches!(g, GenericParam::Type(_)))
        .collect();

    let mut punctuated_generics = Punctuated::<_, Token![,]>::new();
    punctuated_generics.extend(lifetimeless_generics.iter());

    let mut punctuated_generic_idents = Punctuated::<_, Token![,]>::new();
    punctuated_generic_idents.extend(lifetimeless_generics.iter().map(|g| match g {
        GenericParam::Type(g) => &g.ident,
        _ => panic!(),
    }));

    let struct_name = &ast.ident;
    let fetch_struct_name = Ident::new(&format!("{}State", struct_name), Span::call_site());
    let fetch_struct_visibility = &ast.vis;

    TokenStream::from(quote! {
        impl #impl_generics #path::system::SystemParam for #struct_name#ty_generics #where_clause {
            type Fetch = #fetch_struct_name <(#(<#field_types as #path::system::SystemParam>::Fetch,)*), #punctuated_generic_idents>;
        }

        #[doc(hidden)]
        #fetch_struct_visibility struct #fetch_struct_name<TSystemParamState, #punctuated_generic_idents> {
            state: TSystemParamState,
            marker: std::marker::PhantomData<(#punctuated_generic_idents)>
        }

        unsafe impl<TSystemParamState: #path::system::SystemParamState, #punctuated_generics> #path::system::SystemParamState for #fetch_struct_name<TSystemParamState, #punctuated_generic_idents> {
            type Config = TSystemParamState::Config;
            fn init(world: &mut #path::world::World, system_meta: &mut #path::system::SystemMeta, config: Self::Config) -> Self {
                Self {
                    state: TSystemParamState::init(world, system_meta, config),
                    marker: std::marker::PhantomData,
                }
            }

            fn new_archetype(&mut self, archetype: &#path::archetype::Archetype, system_meta: &mut #path::system::SystemMeta) {
                self.state.new_archetype(archetype, system_meta)
            }

            fn default_config() -> TSystemParamState::Config {
                TSystemParamState::default_config()
            }

            fn apply(&mut self, world: &mut #path::world::World) {
                self.state.apply(world)
            }
        }

        impl #impl_generics #path::system::SystemParamFetch<'w, 's> for #fetch_struct_name <(#(<#field_types as #path::system::SystemParam>::Fetch,)*), #punctuated_generic_idents> {
            type Item = #struct_name#ty_generics;
            unsafe fn get_param(
                state: &'s mut Self,
                system_meta: &#path::system::SystemMeta,
                world: &'w #path::world::World,
                change_tick: u32,
            ) -> Self::Item {
                #struct_name {
                    #(#fields: <<#field_types as #path::system::SystemParam>::Fetch as #path::system::SystemParamFetch>::get_param(&mut state.state.#field_indices, system_meta, world, change_tick),)*
                    #(#ignored_fields: <#ignored_field_types>::default(),)*
                }
            }
        }
    })
}

static READONLY_ATTRIBUTE_NAME: &str = "readonly";
static FILTER_ATTRIBUTE_NAME: &str = "filter";

/// Implement `WorldQuery` to use a struct as a parameter in a query
#[proc_macro_derive(Fetch, attributes(readonly, filter))]
pub fn derive_fetch(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let FetchImplTokens {
        struct_name,
        fetch_struct_name,
        state_struct_name,
        fetch_trait_punctuated_lifetimes,
        impl_generics,
        ty_generics,
        where_clause,
        struct_has_world_lt,
        world_lt,
        state_lt,
    } = fetch_impl_tokens(&ast);

    // Fetch's HRTBs require this hack to make the implementation compile. I don't fully understand
    // why this works though. If anyone's curious enough to try to find a better work-around, I'll
    // leave playground links here:
    // - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=da5e260a5c2f3e774142d60a199e854a (this fails)
    // - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=802517bb3d8f83c45ee8c0be360bb250 (this compiles)
    let mut fetch_generics = ast.generics.clone();
    fetch_generics.params.insert(0, state_lt);
    if !struct_has_world_lt {
        fetch_generics.params.insert(0, world_lt);
    }
    fetch_generics
        .params
        .push(GenericParam::Lifetime(LifetimeDef::new(Lifetime::new(
            "'fetch",
            Span::call_site(),
        ))));
    let (fetch_impl_generics, _, _) = fetch_generics.split_for_impl();
    let mut fetch_generics = ast.generics.clone();
    if struct_has_world_lt {
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

    let mut is_read_only = true;

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
            is_read_only: field_is_read_only,
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
        is_read_only = is_read_only && field_is_read_only;
        readonly_types_to_assert.extend(field_readonly_types_to_assert.into_iter());
    }

    let read_only_impl = if is_read_only {
        quote! {
            /// SAFETY: each item in the struct is read only
            unsafe impl #impl_generics #path::query::ReadOnlyFetch for #fetch_struct_name #ty_generics #where_clause {}

            // Statically checks that the safety guarantee holds true indeed. We need this to make
            // sure that we don't compile ReadOnlyFetch if our struct contains nested WorldQuery
            // that don't implement it.
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
                #struct_name {
                    #(#non_filter_field_idents: self.#non_filter_field_idents.table_fetch(_table_row),)*
                    #(#filter_field_idents: self.#filter_field_idents.table_filter_fetch(_table_row),)*
                    #(#phantom_field_idents: Default::default(),)*
                }
            }

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

        // SAFETY: update_component_access and update_archetype_component_access are called for each item in the struct
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

        #read_only_impl
    });
    tokens
}

/// Implement `FilterFetch` to use a struct as a filter parameter in a query
#[proc_macro_derive(FilterFetch)]
pub fn derive_fetch_filter(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let FetchImplTokens {
        struct_name,
        fetch_struct_name,
        state_struct_name,
        fetch_trait_punctuated_lifetimes,
        impl_generics,
        ty_generics,
        where_clause,
        struct_has_world_lt,
        world_lt,
        state_lt,
    } = fetch_impl_tokens(&ast);

    // Fetch's HRTBs require this hack to make the implementation compile. I don't fully understand
    // why this works though. If anyone's curious enough to try to find a better work-around, I'll
    // leave playground links here:
    // - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=da5e260a5c2f3e774142d60a199e854a (this fails)
    // - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=802517bb3d8f83c45ee8c0be360bb250 (this compiles)
    let mut fetch_generics = ast.generics.clone();
    fetch_generics.params.insert(0, state_lt);
    if !struct_has_world_lt {
        fetch_generics.params.insert(0, world_lt);
    }
    fetch_generics
        .params
        .push(GenericParam::Lifetime(LifetimeDef::new(Lifetime::new(
            "'fetch",
            Span::call_site(),
        ))));
    let (fetch_impl_generics, _, _) = fetch_generics.split_for_impl();
    let mut fetch_generics = ast.generics.clone();
    if struct_has_world_lt {
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

#[proc_macro_derive(SystemLabel)]
pub fn derive_system_label(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    derive_label(input, Ident::new("SystemLabel", Span::call_site())).into()
}

#[proc_macro_derive(StageLabel)]
pub fn derive_stage_label(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive_label(input, Ident::new("StageLabel", Span::call_site())).into()
}

#[proc_macro_derive(AmbiguitySetLabel)]
pub fn derive_ambiguity_set_label(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive_label(input, Ident::new("AmbiguitySetLabel", Span::call_site())).into()
}

#[proc_macro_derive(RunCriteriaLabel)]
pub fn derive_run_criteria_label(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive_label(input, Ident::new("RunCriteriaLabel", Span::call_site())).into()
}

fn derive_label(input: DeriveInput, label_type: Ident) -> TokenStream2 {
    let ident = input.ident;
    let ecs_path: Path = bevy_ecs_path();

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let mut where_clause = where_clause.cloned().unwrap_or_else(|| syn::WhereClause {
        where_token: Default::default(),
        predicates: Default::default(),
    });
    where_clause.predicates.push(syn::parse2(quote! { Self: Eq + ::std::fmt::Debug + ::std::hash::Hash + Clone + Send + Sync + 'static }).unwrap());

    quote! {
        impl #impl_generics #ecs_path::schedule::#label_type for #ident #ty_generics #where_clause {
            fn dyn_clone(&self) -> Box<dyn #ecs_path::schedule::#label_type> {
                Box::new(Clone::clone(self))
            }
        }
    }
}

struct FetchImplTokens<'a> {
    struct_name: Ident,
    fetch_struct_name: Ident,
    state_struct_name: Ident,
    fetch_trait_punctuated_lifetimes: Punctuated<GenericParam, Token![,]>,
    impl_generics: ImplGenerics<'a>,
    ty_generics: TypeGenerics<'a>,
    where_clause: Option<&'a WhereClause>,
    struct_has_world_lt: bool,
    world_lt: GenericParam,
    state_lt: GenericParam,
}

fn fetch_impl_tokens(ast: &DeriveInput) -> FetchImplTokens {
    let world_lt = ast.generics.params.first().and_then(|param| match param {
        lt @ GenericParam::Lifetime(_) => Some(lt.clone()),
        _ => None,
    });
    let struct_has_world_lt = world_lt.is_some();
    let world_lt = world_lt.unwrap_or_else(|| {
        GenericParam::Lifetime(LifetimeDef::new(Lifetime::new("'world", Span::call_site())))
    });
    let state_lt =
        GenericParam::Lifetime(LifetimeDef::new(Lifetime::new("'state", Span::call_site())));

    let mut fetch_trait_punctuated_lifetimes = Punctuated::<_, Token![,]>::new();
    fetch_trait_punctuated_lifetimes.push(world_lt.clone());
    fetch_trait_punctuated_lifetimes.push(state_lt.clone());

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
        struct_has_world_lt,
        world_lt,
        state_lt,
    }
}

struct WorldQueryFieldTypeInfo {
    /// We convert `Mut<T>` to `&mut T` (because this is the type that implements `WorldQuery`)
    /// and store it here.
    query_type: Type,
    /// The same as `query_type` but with `'fetch` lifetime.
    fetch_init_type: Type,
    is_read_only: bool,
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
    let mut is_read_only = true;
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
                        assert_not_generic(&path, generic_names);

                        let segment = path.path.segments.last().unwrap();
                        let ty_ident = &segment.ident;
                        if ty_ident == "Mut" {
                            is_read_only = false;
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
                is_read_only = false;
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
                assert_not_generic(&path, generic_names);

                match &mut path_init.path.segments.last_mut().unwrap().arguments {
                    PathArguments::AngleBracketed(args) => {
                        match args.args.first_mut() {
                            Some(GenericArgument::Lifetime(lt)) => {
                                *lt = Lifetime::new("'fetch", Span::call_site());
                            }
                            _ => {},
                        }
                    }
                    _ => {},
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
                        is_read_only = false;
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
                    is_read_only: elem_is_read_only,
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
                is_read_only = is_read_only && elem_is_read_only;
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

    return WorldQueryFieldTypeInfo {
        query_type,
        fetch_init_type,
        is_read_only,
        is_phantom,
        readonly_types_to_assert,
    };
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

fn bevy_ecs_path() -> syn::Path {
    BevyManifest::default().get_path("bevy_ecs")
}
