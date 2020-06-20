extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, DeriveInput, Field, Fields, GenericArgument, Ident, Lit,
    Meta, NestedMeta, PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), name.span());
    let data = input.data;

    if let syn::Data::Struct(data) = data {
        let (fields, errors): (_, Vec<_>) = match data.fields {
            Fields::Named(ref fields) => fields.named.iter(),
            _ => unimplemented!(),
        }
        .map(|f| match check_each(f) {
            Ok(check_each) => Ok((f, check_option(&f.ty), check_each)),
            Err(err) => Err(err.to_compile_error()),
        })
        .partition(Result::is_ok);

        if errors.len() > 0 {
            let errors = errors.into_iter().filter_map(Result::err);
            return TokenStream::from(quote! {
                #(#errors)*
            });
        }

        let fields = fields.into_iter().filter_map(Result::ok);

        let fields_types = fields.clone().map(|(f, check_option, check_each)| {
            let name = &f.ident;
            let ty = &f.ty;
            if let Some(ty) = check_option {
                quote_spanned! {f.span()=>
                    #name: std::option::Option<#ty>,
                }
            } else if let Some((_, ty)) = check_each {
                quote_spanned! {f.span() =>
                    #name: std::vec::Vec<#ty>,
                }
            } else {
                quote_spanned! {f.span() =>
                    #name: std::option::Option<#ty>,
                }
            }
        });

        let fields_init = fields.clone().map(|(f, _, check_each)| {
            let name = &f.ident;
            if check_each.is_some() {
                quote_spanned! {f.span() =>
                    #name: vec![],
                }
            } else {
                quote_spanned! {f.span() =>
                    #name: std::option::Option::None,
                }
            }
        });

        let impl_fns = fields.clone().map(|(f, check_option, check_each)| {
            let name = &f.ident;
            let ty = &f.ty;
            if let Some(ty) = check_option {
                quote_spanned! {f.span() =>
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = std::option::Option::Some(#name);
                        self
                    }
                }
            } else if let Some((i_name, i_ty)) = check_each {
                if name.is_some() && &i_name != name.as_ref().unwrap() {
                    quote_spanned! {f.span() =>
                        fn #i_name(&mut self, #i_name: #i_ty) -> &mut Self {
                            self.#name.push(#i_name);
                            self
                        }
                        fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = #name;
                            self
                        }
                    }
                } else {
                    quote_spanned! {f.span() =>
                        fn #i_name(&mut self, #i_name: #i_ty) -> &mut Self {
                            self.#name.push(#i_name);
                            self
                        }
                    }
                }
            } else {
                quote_spanned! {f.span() =>
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = std::option::Option::Some(#name);
                        self
                    }
                }
            }
        });

        let none_check = fields.clone().map(|(f, check_option, check_each)| {
            let name = &f.ident;
            if check_option.is_none() && check_each.is_none() {
                quote! {
                    if self.#name.is_none() {
                        return Err(format!("field not set").into());
                    }
                }
            } else {
                quote! {}
            }
        });

        let fields_unwrap = fields.map(|(f, check_option, check_each)| {
            let name = &f.ident;
            if check_option.is_none() && check_each.is_none() {
                quote! {
                    #name: self.#name.clone().unwrap(),
                }
            } else {
                quote! {
                    #name: self.#name.clone(),
                }
            }
        });

        let expanded = quote! {
            pub struct #builder_name {
                #(#fields_types)*
            }

            impl #name {
                pub fn builder() -> #builder_name {
                    #builder_name {
                        #(#fields_init)*
                    }
                }
            }

            impl #builder_name {
                #(#impl_fns)*

                pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                    #(#none_check)*

                    Ok(#name {
                        #(#fields_unwrap)*
                    })
                }
            }
        };

        TokenStream::from(expanded)
    } else {
        unimplemented!()
    }
}

fn check_option(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Path(type_path) => {
            if type_path.qself.is_none() {
                let segments = &type_path.path.segments;
                if segments.len() == 1 {
                    let first = segments.first().unwrap();
                    if first.ident == Ident::new("Option", first.ident.span()) {
                        match &first.arguments {
                            PathArguments::AngleBracketed(args) => {
                                let args = &args.args;
                                if args.len() == 1 {
                                    let first = args.first().unwrap();
                                    match first {
                                        GenericArgument::Type(ty) => Some(ty),
                                        _ => None,
                                    }
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

fn check_vec(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Path(type_path) => {
            if type_path.qself.is_none() {
                let segments = &type_path.path.segments;
                if segments.len() == 1 {
                    let first = segments.first().unwrap();
                    if first.ident == Ident::new("Vec", first.ident.span()) {
                        match &first.arguments {
                            PathArguments::AngleBracketed(args) => {
                                let args = &args.args;
                                if args.len() == 1 {
                                    let first = args.first().unwrap();
                                    match first {
                                        GenericArgument::Type(ty) => Some(ty),
                                        _ => None,
                                    }
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

fn check_each(f: &Field) -> Result<Option<(Ident, &Type)>, syn::Error> {
    let check_vec = check_vec(&f.ty);
    if let Some(vec_type) = check_vec {
        let attrs = &f.attrs;
        for attr in attrs {
            let meta = attr.parse_meta().unwrap();
            match meta {
                Meta::List(meta_list) => {
                    let segments = &meta_list.path.segments;
                    if segments.len() == 1 {
                        let first = segments.first().unwrap();
                        if first.ident == Ident::new("builder", first.span()) {
                            let nested_list = meta_list.nested;
                            if nested_list.len() == 1 {
                                let first = nested_list.first().unwrap();
                                match first {
                                    NestedMeta::Meta(Meta::NameValue(name_value)) => {
                                        let segments = &name_value.path.segments;
                                        if segments.len() == 1 {
                                            let first = segments.first().unwrap();
                                            if first.ident == Ident::new("each", first.span()) {
                                                if let Lit::Str(lit) = &name_value.lit {
                                                    let ident =
                                                        Ident::new(&lit.value(), lit.span());
                                                    return Ok(Some((ident, vec_type)));
                                                }
                                            } else {
                                                return Err(syn::Error::new(
                                                    first.ident.span(),
                                                    "expected `builder(each = \"...\")`",
                                                ));
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }
    Ok(None)
}
