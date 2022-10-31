use serde::{Deserialize, Serialize};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

pub trait Wrapper {
    type Inner: PartialEq;
    type Error: Display;
    fn new<T: Into<Self::Inner>>(inner: T) -> Result<Self, Self::Error>
    where
        Self: Sized;
    fn inner(&self) -> &Self::Inner;
    fn unwrap(self) -> Self::Inner;
}

#[derive(Debug, Default)]
struct Erroneous {}

impl Display for Erroneous {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "erroneous")
    }
}

impl Error for Erroneous {}

macro_rules! impl_refs_for {
    ($for:ident, $for_str:expr) => {
        impl TryFrom<<$for as Wrapper>::Inner> for $for {
            type Error = <$for as Wrapper>::Error;

            fn try_from(input: <$for as Wrapper>::Inner) -> Result<Self, Self::Error> {
                Self::new(input)
            }
        }

        impl From<$for> for <$for as Wrapper>::Inner {
            fn from(wrapper: $for) -> Self {
                wrapper.unwrap()
            }
        }

        impl<'de> serde::de::Deserialize<'de> for $for {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::de::Deserializer<'de>,
            {
                let original = <$for as Wrapper>::Inner::deserialize(deserializer)?;
                // this is dumb
                use serde::de::Error;
                let wrapper = $for::new(original).map_err(|e| {
                    D::Error::custom(format!("Unable to deserialize into {}: {}", $for_str, e))
                })?;
                Ok(wrapper)
            }
        }

        // We want to serialize the inner value back out, not our structure, which is just
        // there to force validation.
        impl serde::ser::Serialize for $for {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::ser::Serializer,
            {
                self.inner().serialize(serializer)
            }
        }

        impl std::ops::Deref for $for {
            type Target = <$for as Wrapper>::Inner;
            fn deref(&self) -> &Self::Target {
                self.inner()
            }
        }

        impl core::borrow::Borrow<<$for as Wrapper>::Inner> for $for {
            fn borrow(&self) -> &<$for as Wrapper>::Inner {
                self.inner()
            }
        }

        impl AsRef<<$for as Wrapper>::Inner> for $for {
            fn as_ref(&self) -> &<$for as Wrapper>::Inner {
                self.inner()
            }
        }

        impl std::fmt::Display for $for {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.inner())
            }
        }

        impl PartialEq<<$for as Wrapper>::Inner> for $for {
            fn eq(&self, other: &<$for as Wrapper>::Inner) -> bool {
                self.inner().eq(other)
            }
        }

        impl PartialEq<$for> for <$for as Wrapper>::Inner {
            fn eq(&self, other: &$for) -> bool {
                self.eq(other.inner())
            }
        }

        impl PartialEq<&<$for as Wrapper>::Inner> for $for {
            fn eq(&self, other: &&<$for as Wrapper>::Inner) -> bool {
                self.inner().eq(*other)
            }
        }

        impl PartialEq<&$for> for <$for as Wrapper>::Inner {
            fn eq(&self, other: &&$for) -> bool {
                self.eq(other.inner())
            }
        }

        impl PartialEq<<$for as Wrapper>::Inner> for &$for {
            fn eq(&self, other: &<$for as Wrapper>::Inner) -> bool {
                self.inner().eq(other)
            }
        }

        impl PartialEq<$for> for &<$for as Wrapper>::Inner {
            fn eq(&self, other: &$for) -> bool {
                (*self).eq(other.inner())
            }
        }
    };
}

// These assume that the "inner" type implements AsRef<str>, et. al. (i.e. when it is a `String`)
macro_rules! impl_as_ref_str_for {
    ($for:ident) => {
        impl AsRef<str> for $for {
            fn as_ref(&self) -> &str {
                self.inner().as_ref()
            }
        }

        impl core::borrow::Borrow<str> for $for {
            fn borrow(&self) -> &str {
                self.inner().as_ref()
            }
        }
    };
}

#[derive(Debug, PartialEq)]
struct Int {
    inner: u64,
}

impl Wrapper for Int {
    type Inner = u64;
    type Error = Erroneous;

    fn new<T: Into<Self::Inner>>(inner: T) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let inner = inner.into();
        if inner == 5 {
            Err(Erroneous {})
        } else {
            Ok(Self { inner })
        }
    }

    fn inner(&self) -> &Self::Inner {
        &self.inner
    }

    fn unwrap(self) -> Self::Inner {
        self.inner
    }
}

impl_refs_for!(Int, "Int");

#[derive(Debug, PartialEq)]
struct Str {
    inner: String,
}

impl Wrapper for Str {
    type Inner = String;
    type Error = Erroneous;

    fn new<T: Into<Self::Inner>>(inner: T) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let inner = inner.into();
        if inner.to_lowercase() == "bones" {
            Err(Erroneous {})
        } else {
            Ok(Self { inner })
        }
    }

    fn inner(&self) -> &Self::Inner {
        &self.inner
    }

    fn unwrap(self) -> Self::Inner {
        self.inner
    }
}

impl_refs_for!(Str, "Str");
impl_as_ref_str_for!(Str);

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum Enumious {
    AlphaDay,
    BetaNight,
    GammaTwilight,
    #[serde(rename = "SURPRISE")]
    DeltaDawn,
}

// TODO - figure out how to make this easy
impl Display for Enumious {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fake")
    }
}

#[derive(Debug, PartialEq)]
struct EnumSetting {
    inner: Enumious,
}

impl Wrapper for EnumSetting {
    type Inner = Enumious;
    type Error = Erroneous;

    fn new<T: Into<Self::Inner>>(inner: T) -> Result<Self, Self::Error>
    where
        Self: Sized,
    {
        let inner = inner.into();
        match inner {
            Enumious::GammaTwilight => Err(Erroneous {}),
            _ => Ok(Self { inner }),
        }
    }

    fn inner(&self) -> &Self::Inner {
        &self.inner
    }

    fn unwrap(self) -> Self::Inner {
        self.inner
    }
}

impl_refs_for!(EnumSetting, "EnumSetting");

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Settings {
    a: EnumSetting,
    b: EnumSetting,
    c: Int,
    d: Str,
}

#[cfg(test)]
fn takes_as_ref_str<S: AsRef<str>>(s: S) -> String {
    s.as_ref().to_string()
}

#[test]
fn test_1() {
    let err = Str::new("bones");
    assert!(err.is_err())
}

#[test]
fn test_2() {
    let wrapper = Str::new("bishop").unwrap();
    let s1: &str = wrapper.as_ref();
    let s2: &String = wrapper.as_ref();
    assert_eq!("bishop", format!("{}", s1));
    assert_eq!("bishop", format!("{}", s2));
    assert_eq!("bishop", format!("{}", wrapper));
    assert_eq!("bishop", takes_as_ref_str(wrapper))
}

#[test]
fn test_3() {
    let err = Int::new(5u64);
    assert!(err.is_err())
}

#[test]
fn test_4() {
    let wrapper = Int::new(1u64).unwrap();
    let x: &u64 = wrapper.as_ref();
    assert_eq!(1, *x);
}

#[test]
fn test_partial_eq() {
    let a = Int::new(1u64).unwrap();
    let b = 1u64;
    let eq = a == b;
    assert!(eq);
    let eq = &a == b;
    assert!(eq);
    let eq = b == a;
    assert!(eq);
    let eq = &b == a;
    assert!(eq);
    let eq = &a == &b;
    assert!(eq);
}

#[test]
fn test_serde() {
    let json: serde_json::Value = serde_json::json!(
    {
        "a": "SURPRISE",
        "b": "ALPHA_DAY",
        "c": 36,
        "d": "foo"
    });
    let settings: Settings = serde_json::from_value(json).unwrap();
    let expected = Settings {
        a: EnumSetting::new(Enumious::DeltaDawn).unwrap(),
        b: EnumSetting::new(Enumious::AlphaDay).unwrap(),
        c: Int::new(36u64).unwrap(),
        d: Str::new("foo").unwrap(),
    };
    assert_eq!(settings, expected)
}

#[test]
fn test_invalid_value() {
    let json: serde_json::Value = serde_json::json!(
    {
        "a": "SURPRISE",
        "b": "ALPHA_DAY",
        "c": 5,
        "d": "foo"
    });
    let err = serde_json::from_value::<Settings>(json);
    assert!(err.is_err());
}
