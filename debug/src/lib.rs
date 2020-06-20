extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, parse_quote, punctuated::Punctuated};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    let name = &input.ident;
    let name_q = format!("{}", input.ident);
    let fields = match input.data {
        syn::Data::Struct(data_struct) => data_struct.fields,
        _ => unimplemented!(),
    };

    let generics = add_trait_bounds(input.generics, &fields, &input.attrs);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields = match &fields {
        syn::Fields::Named(named_fields) => named_fields.named.iter(),
        _ => unimplemented!(),
    }
    .map(|f| expand_field(f));

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#name_q)
                #(#fields)*
                .finish()
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn add_trait_bounds(
    mut generics: syn::Generics,
    fields: &syn::Fields,
    attrs: &Vec<syn::Attribute>,
) -> syn::Generics {
    let mut where_clause = generics.where_clause.unwrap_or(syn::WhereClause {
        where_token: syn::token::Where {
            span: Span::call_site(),
        },
        predicates: Punctuated::new(),
    });

    for attr in attrs {
        if let Ok(syn::Meta::List(meta_list)) = attr.parse_meta() {
            if let Some(ident) = meta_list.path.get_ident() {
                if ident == "debug" {
                    if meta_list.nested.len() == 1 {
                        let first = meta_list.nested.first().unwrap();
                        if let syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) = first {
                            if name_value.path.get_ident().unwrap() == "bound" {
                                if let syn::Lit::Str(lit_str) = &name_value.lit {
                                    where_clause
                                        .predicates
                                        .push(syn::parse_str(&lit_str.value()).unwrap());
                                    generics.where_clause = Some(where_clause);
                                    return generics;
                                } else {
                                    unimplemented!()
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            let mut should_add = true;
            for f in fields {
                let ty_ident = &type_param.ident;
                let ph_ty: syn::Type = parse_quote! {PhantomData<#ty_ident>};
                if f.ty == ph_ty {
                    should_add = false;
                } else if let syn::Type::Path(type_path) = &f.ty {
                    let segments = &type_path.path.segments;
                    if segments.len() == 1 {
                        let arguments = &segments.first().unwrap().arguments;
                        if let syn::PathArguments::AngleBracketed(args) = arguments {
                            for arg in &args.args {
                                if let syn::GenericArgument::Type(syn::Type::Path(ref type_path)) =
                                    arg
                                {
                                    let segments = &type_path.path.segments;
                                    if segments.len() >= 1 {
                                        if segments.first().unwrap().ident == type_param.ident {
                                            should_add = false;
                                            where_clause
                                                .predicates
                                                .push(parse_quote!(#type_path: Debug));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if should_add {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics.where_clause = Some(where_clause);
    generics
}

fn expand_field(f: &syn::Field) -> TokenStream {
    let debug_fmt = extract_debug(f);
    let name = &f.ident;
    let name_q = format!("{}", &f.ident.as_ref().unwrap());
    quote! {
        .field(#name_q, &std::format_args!(#debug_fmt, &self.#name))
    }
}

fn extract_debug(f: &syn::Field) -> TokenStream {
    let attrs = &f.attrs;
    for attr in attrs {
        if let Ok(syn::Meta::NameValue(name_value)) = attr.parse_meta() {
            if let Some(ident) = name_value.path.get_ident() {
                if ident == "debug" {
                    if let syn::Lit::Str(lit_str) = name_value.lit {
                        return quote! {#lit_str};
                    } else {
                        let error = syn::Error::new(ident.span(), "Expected string literal")
                            .to_compile_error();
                        return quote! {
                            #error
                        };
                    }
                }
            }
        }
    }
    quote! {"{:?}"}
}
