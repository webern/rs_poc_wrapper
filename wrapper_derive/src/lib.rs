use darling::FromAttributes;
use proc_macro::TokenStream;
use quote::{format_ident, ToTokens};
use quote::{quote, TokenStreamExt};
use syn::__private::TokenStream2;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Field, Fields};

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
    _raw_settings: RawSettings,
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
            _raw_settings: settings,
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
                format!("{}", inner)
            }
        );
        let impls = quote!(
            impl wrapper::Wrapper for #wrapper {
                type Inner = #inner;

                fn new<T: Into<Self::Inner>>(inner: T) -> Result<Self, wrapper::ValidationError>
                where
                    Self: Sized,
                {
                    Ok(<#wrapper as wrapper::Validate>::validate(inner.into())?)
                }

                fn inner(&self) -> &Self::Inner { &self.#inner_field_name }

                fn unwrap(self) -> Self::Inner { self.#inner_field_name }
            }

            impl TryFrom<<#wrapper as Wrapper>::Inner> for #wrapper {
                type Error = ValidationError;
                fn try_from(input: <#wrapper as Wrapper>::Inner) -> Result<Self, ValidationError> {
                    Self::new(input)
                }
            }

            impl<'de> serde::de::Deserialize<'de> for #wrapper {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: serde::de::Deserializer<'de>,
                {
                    let original = <#wrapper as Wrapper>::Inner::deserialize(deserializer)?;
                    // We need to make sure the serde Error trait is in scope.
                    use serde::de::Error as _;
                    let wrapper = #wrapper::new(original).map_err(|e| {
                        D::Error::custom(format!("Unable to deserialize into {}: {}", stringify!(#wrapper), e))
                    })?;
                    Ok(wrapper)
                }
            }

            impl serde::ser::Serialize for #wrapper {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::ser::Serializer,
                {
                    self.inner().serialize(serializer)
                }
            }

            impl std::fmt::Display for #wrapper {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.inner())
                }
            }

            impl std::ops::Deref for #wrapper {
                type Target = #inner_ref_type;
                fn deref(&self) -> &Self::Target {
                    self.inner()
                }
            }

            impl core::borrow::Borrow<#inner_ref_type> for #wrapper {
                fn borrow(&self) -> &#inner_ref_type {
                    self.inner()
                }
            }

            impl AsRef<#inner_ref_type> for #wrapper {
                fn as_ref(&self) -> &#inner_ref_type {
                    self.inner()
                }
            }

            // The Wrapper type can be compared with the Inner type
            impl PartialEq<<#wrapper as Wrapper>::Inner> for #wrapper {
                fn eq(&self, other: &<#wrapper as Wrapper>::Inner) -> bool {
                    self.inner().eq(other)
                }
            }

            // The Inner type can be compared with the Wrapper type
            impl PartialEq<#wrapper> for <#wrapper as Wrapper>::Inner {
                fn eq(&self, other: &#wrapper) -> bool {
                    self.eq(other.inner())
                }
            }

            impl PartialEq<&#inner_ref_type> for #wrapper {
                fn eq(&self, other: &&#inner_ref_type) -> bool {
                    self.inner().eq(*other)
                }
            }

            impl PartialEq<&#wrapper> for <#wrapper as Wrapper>::Inner {
                fn eq(&self, other: &&#wrapper) -> bool {
                    self.eq(other.inner())
                }
            }

            impl PartialEq<<#wrapper as Wrapper>::Inner> for &#wrapper {
                fn eq(&self, other: &<#wrapper as Wrapper>::Inner) -> bool {
                    self.inner().eq(other)
                }
            }

            impl PartialEq<#wrapper> for &#inner_ref_type {
                fn eq(&self, other: &#wrapper) -> bool {
                    (*self).eq(other.inner())
                }
            }
        );

        stream.append_all(impls.into_iter());

        if self.as_ref_str {
            let code = quote!(
                impl TryFrom<&#inner_ref_type> for #wrapper {
                    type Error = ValidationError;
                    fn try_from(input: &#inner_ref_type) -> Result<Self, ValidationError> {
                        Self::new(input)
                    }
                }
            );
            stream.append_all(code.into_iter());
        }
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
