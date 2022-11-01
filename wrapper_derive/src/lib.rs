use darling::{FromAttributes, FromMeta};
use proc_macro::{Ident, TokenStream};
use quote::ToTokens;
use std::io::Write;
use syn::Lit::Str;
use syn::{
    parse_macro_input, parse_quote, Attribute, AttributeArgs, Data, DataStruct, DeriveInput, Field,
    Fields, ItemStruct, Type, Visibility,
};

// #[cfg(debug_assertions)]
// fn debug<S: AsRef<str>>(s: S) {
//     let p = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("log.debug");
//     let mut f = std::fs::OpenOptions::new().write(true).open(&p).expect(&format!("unable to open '{}'", p.display()));
//     f.write_all(s.as_ref().as_bytes()).expect(&format!("unable to write to '{}'", p.display()));
// }

#[proc_macro_derive(Wrapper, attributes(wrapper))]
pub fn wrapper(input: TokenStream) -> TokenStream {
    // Parse the input tokens
    // let input_cloned = input;
    let derive_input = parse_macro_input!(input as DeriveInput);
    let settings = RawSettings::from_attributes(derive_input.attrs.as_slice())
        .expect("Unable to parse `wrapper` macro arguments");
    // let attr_args = parse_macro_input!(input as AttributeArgs);

    let info = StructInfo::new(&derive_input, settings);

    panic!("{:?}", info);

    // panic!("{:?}\n\n{:?}", derive_input, "attr_args")
}

/// Store any args given by the user inside `#[wrapper(as_ref_str=false, ...)]`.
#[derive(Debug, Clone, Default, FromAttributes)]
#[darling(attributes(wrapper))]
#[darling(default)]
struct RawSettings {
    as_ref_str: Option<bool>,
    inner: Option<String>,
}

#[derive(Debug, Clone)]
struct StructInfo {
    settings: RawSettings,
    ident: Ident,
}

impl StructInfo {
    fn new(derive_input: &DeriveInput, settings: RawSettings) -> Self {
        let struct_ident = derive_input.ident.clone();
        let (inner_field, inner_field_name) = match derive_input.data.clone() {
            Data::Struct(s) => find_inner_field(s, settings.inner.as_ref().map(|s| s.as_str())),
            Data::Enum(_) => panic!("A Wrapper cannot be an enum, it must be a struct"),
            Data::Union(_) => panic!("A Wrapper cannot be an union, it must be a struct"),
        };
        let fucking_ident: syn::Ident = inner_field.ident.unwrap();
        let fucking_ty = inner_field.ty;
        let mut stream = proc_macro2::TokenStream::new();
        fucking_ty.to_tokens(&mut stream);
        let s = stream.to_string();
        let as_ref_str = settings.as_ref_str.unwrap_or(s == "String");
        panic!("{}", s);

        // let dlkfdfklgj = match fucking_ty {
        //     Type::Array(_) => {}
        //     Type::BareFn(_) => {}
        //     Type::Group(_) => {}
        //     Type::ImplTrait(_) => {}
        //     Type::Infer(_) => {}
        //     Type::Macro(_) => {}
        //     Type::Never(_) => {}
        //     Type::Paren(_) => {}
        //     Type::Path(x) => {panic!("{:?}", x)}
        //     Type::Ptr(_) => {}
        //     Type::Reference(_) => {}
        //     Type::Slice(_) => {}
        //     Type::TraitObject(_) => {}
        //     Type::Tuple(_) => {}
        //     Type::Verbatim(_) => {}
        //     _ => {}
        // };

        // panic!("{}", );
        todo!("xyz")
    }
}

/// This function finds the field of the struct that has the "inner" type, which has been "wrapped"
/// by the struct. The most common example is:
///
/// ```no_run
/// struct SomeStruct {
///     inner: String
/// }
/// ```
///
/// In the above example, this function would return the "inner" field and its name.
fn find_inner_field(data_struct: DataStruct, field_name: Option<&str>) -> (Field, String) {
    match &data_struct.fields {
        Fields::Named(named_fields) => {
            let field_name = field_name.unwrap_or("inner");
            for field in &named_fields.named {
                if let Some(field_ident) = &field.ident {
                    let field_ident: &syn::Ident = &field_ident;
                    if field_ident == field_name {
                        return (field.clone(), field_name.to_string());
                    }
                }
            }
            panic!(
                "The Wrapper derive macro could not find a field named '{}', in this struct",
                field_name
            );
        }
        Fields::Unnamed(unnamed_field) => {
            let field_name = field_name.unwrap_or("0");
            if field_name != "0" {
                panic!("The Wrapper derive found a struct with unnamed fields, but the field name '{}' was given. \
                Do not pass a value for 'inner' when using a struct with unnamed fields. \
                The only valid inner field name is '0'", field_name);
            }
            return (
                unnamed_field
                    .unnamed
                    .iter()
                    .next()
                    .expect("The Wrapper could not parse the unnamed fields of this struct")
                    .clone(),
                field_name.to_string(),
            );
        }
        Fields::Unit => {
            panic!("A Wrapper cannot be of type 'unit', it should have one or more fields")
        }
    }
}
