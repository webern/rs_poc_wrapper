use wrapper::{Validate, ValidationError, Wrapper};
use wrapper_derive::Wrapper;

#[derive(Wrapper)]
#[wrapper(as_ref_str = true)]
struct Pizza {
    inner: String,
}

impl Validate for Pizza {
    fn validate(inner: <Self as Wrapper>::Inner) -> Result<Self, ValidationError> {
        if inner.to_lowercase().contains("pineapple") {
            Err(ValidationError::new(
                "Failure validating Pizza: pineapple is gross",
            ))
        } else {
            Ok(Pizza { inner })
        }
    }
}

#[test]
fn test() {}
// #[derive(Wrapper)]
// struct Unnamed(u64);
