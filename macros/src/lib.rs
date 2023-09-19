extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{parse_macro_input, DeriveInput, Data, LitStr};
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

const TYPE_OPTION: &[&'static str] = &["Option", "std:option:Option", "core:option:Option"];
const TYPE_VEC: &[&'static str] = &["Vec"];

fn extract_type_from_option<'a>(ty: &'a syn::Type, kinds: &'static [&'static str]) -> Option<&'a syn::Type> {
    // If it is not `TypePath`, it is not possible to be `Option<T>`, return `None`
    if let syn::Type::Path(syn::TypePath { qself: None, path }) = ty {
        // We have limited the 5 ways to write `Option`, and we can see that after `Option`,
        // there will be no `PathSegment` of the same level
        // Therefore, we only need to take out the highest level `PathSegment` and splice it into a string
        // for comparison with the analysis result
        let segments_str = &path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>()
            .join(":");
        // Concatenate `PathSegment` into a string, compare and take out the `PathSegment` where `Option` is located
        let option_segment = kinds
            .iter()
            .find(|s| segments_str == *s)
            .and_then(|_| path.segments.last());
        let inner_type = option_segment
            // Take out the generic parameters of the `PathSegment` where `Option` is located
            // If it is not generic, it is not possible to be `Option<T>`, return `None`
            // But this situation may not occur
            .and_then(|path_seg| match &path_seg.arguments {
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                                                       args,
                                                       ..
                                                   }) => args.first(),
                _ => None,
            })
            // Take out the type information in the generic parameter
            // If it is not a type, it is not possible to be `Option<T>`, return `None`
            // But this situation may not occur
            .and_then(|generic_arg| match generic_arg {
                syn::GenericArgument::Type(ty) => Some(ty),
                _ => None,
            });
        // Return `T` in `Option<T>`
        return inner_type;
    }
    None
}

/// Example of user-defined [derive mode macro][1]
///
/// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#derive-mode-macros
#[proc_macro_derive(FromDirective, attributes(dirignore, name))]
pub fn my_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let data = match input.data {
        Data::Struct(s) => s,
        _ => panic!(),
    };
    let ident = &input.ident;

    let mut flets = vec![];
    let mut fmaps = vec![];
    let mut fsets = vec![];

    let mut defs = vec![];
    for field in data.fields {
        let name = field.ident.unwrap();

        let sname = if let Some(attr) = field.attrs.iter().find(|it| it.path().is_ident("name")) {
            attr.parse_args::<LitStr>().unwrap()
        } else {
            LitStr::new(&name.to_string(), name.span())
        };

        if field.attrs.iter().any(|it| it.path().is_ident("dirignore")) {
            defs.push(quote!{ #name: Default::default(), });
            continue;
        }

        if let Some(ty) = extract_type_from_option(&field.ty, TYPE_OPTION) {
            flets.push(quote_spanned!(ty.span() =>
                let mut #name: Option<#ty> = None;
            ));

            fmaps.push(quote_spanned!(ty.span() =>
                #sname => #name = Some(FromValue::from_value(val)),
            ));

            fsets.push(quote_spanned!(ty.span() =>
                #name: #name,
            ));
        } else {
            let ty = field.ty;
            flets.push(quote_spanned!(ty.span() =>
                let mut #name: Option<#ty> = None;
            ));

            fmaps.push(quote_spanned!(ty.span() =>
                #sname => #name = Some(FromValue::from_value(val)),
            ));

            fsets.push(quote_spanned!(ty.span() =>
                #name: #name.unwrap(),
            ));
        }
    }


    let out = quote_spanned!(input.ident.span() =>
         impl<'a, T: Text<'a>> From<&graphql_parser::schema::Directive<'a, T>> for #ident<'a, T> {
            fn from(value: &graphql_parser::schema::Directive<'a, T>) -> Self {
                #(#flets)*

                for (_key, val) in &value.arguments {
                    match _key.as_ref() {
                        #(#fmaps)*
                        _ => {},
                    }
                }

                Self {
                    #(#fsets)*
                    #(#defs)*
                }

            }
        }
    ).into();
    // panic!("{}", out);
    out
}
