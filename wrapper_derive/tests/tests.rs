use wrapper_derive::Wrapper;

#[derive(Wrapper)]
#[wrapper(as_ref_str = true)]
struct Pizza {
    inner: String,
}

#[test]
fn test() {}
// #[derive(Wrapper)]
// struct Unnamed(u64);
