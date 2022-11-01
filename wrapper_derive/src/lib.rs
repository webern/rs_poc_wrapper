use darling::{FromAttributes, FromMeta};
use proc_macro::{Ident, TokenStream};
use quote::{format_ident, ToTokens};
use quote::{quote, TokenStreamExt};
use std::io::Write;
use syn::Lit::Str;
use syn::__private::TokenStream2;
use syn::{
    parse_macro_input, parse_quote, Attribute, AttributeArgs, Data, DataStruct, DeriveInput, Field,
    Fields, ItemStruct, Type, Visibility,
};

#[proc_macro_derive(Wrapper, attributes(wrapper))]
pub fn wrapper(input: TokenStream) -> TokenStream {
    // Parse the input tokens
    let derive_input = parse_macro_input!(input as DeriveInput);
    let settings = RawSettings::from_attributes(derive_input.attrs.as_slice())
        .expect("Unable to parse `wrapper` macro arguments");

    // Further parse the input
    let struct_info = StructInfo::new(&derive_input, settings);

    // Write impls
    let mut ast2 = TokenStream2::new();
    struct_info.write_impls(&mut ast2);
    ast2.into_token_stream().into()
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
    raw_settings: RawSettings,
    wrapper: String,
    inner_field_name: String,
    inner: String,
    as_ref_str: bool,
}

impl StructInfo {
    fn new(derive_input: &DeriveInput, settings: RawSettings) -> Self {
        let wrapper = derive_input.ident.to_string();

        // let struct_typename = struct_ident.
        let (inner_field, inner_field_name) = match derive_input.data.clone() {
            Data::Struct(s) => find_inner_field(s, settings.inner.as_ref().map(|s| s.as_str())),
            Data::Enum(_) => panic!("A Wrapper cannot be an enum, it must be a struct"),
            Data::Union(_) => panic!("A Wrapper cannot be an union, it must be a struct"),
        };
        let inner_type = &inner_field.ty;
        let mut stream = proc_macro2::TokenStream::new();
        inner_type.to_tokens(&mut stream);
        let inner = stream.to_string();
        // Automatically imple AsRef<str> when unspecified by the user but the inner type is String.
        // Note, this might not work if String is not what we think it is.
        let as_ref_str = settings.as_ref_str.unwrap_or(inner == "String");

        Self {
            raw_settings: settings,
            wrapper,
            inner_field_name,
            inner,
            as_ref_str,
        }
    }

    fn write_impls(&self, stream: &mut TokenStream2) {
        let wrapper = format_ident!("{}", &self.wrapper);
        let inner = format_ident!("{}", &self.inner);
        let inner_field_name = format_ident!("{}", &self.inner_field_name);
        let inner_ref_type = format_ident!(
            "{}",
            if self.as_ref_str {
                String::from("str")
            } else {
                format!("&{}", inner)
            }
        );
        let code = quote!(
            impl wrapper::Wrapper for #wrapper {
                type Inner = #inner;
                type Error = wrapper::Erroneous;

                fn new<T: Into<Self::Inner>>(inner: T) -> Result<Self, Self::Error>
                where
                    Self: Sized,
                {
                    let inner = inner.into();
                    // TODO - use a Validate::validate trait
                    Ok(Self{ #inner_field_name: inner })
                }

                fn inner(&self) -> &Self::Inner { &self.#inner_field_name }

                fn unwrap(self) -> Self::Inner { self.#inner_field_name }
            }
        );
        // panic!("{}", code);
        stream.append_all(code.into_iter());
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
