use wrapper_derive::Wrapper;

#[derive(Wrapper)]
#[wrapper(as_ref_str=true)]
struct Pizza {
    inner: String,
}

// #[derive(Wrapper)]
// struct Unnamed(u64);
