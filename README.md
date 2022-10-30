
# Wrapped

This is prep work for fixing Bottlerocket https://github.com/bottlerocket-os/bottlerocket/issues/2257.
In this POC I've demonstrated for myself a way to use a `Wrapped` trait to represent what is currently always...

```rust
struct Something {
    inner: String
}
```

...in Bottlerocket.

The idea is to replace `String` with any type `T` (with some trait bounds).

Currently, I do the "impls" with macro_rules because that is *way* easier than a procedural macro.

Also, there are some follow-ups, like getting `AsRef<str>` to work when the inner type is `AsRef<str>`.
