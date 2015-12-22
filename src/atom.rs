use std::fmt;
use std::borrow::Cow;
use super::token::is_token_delim;

#[derive(Debug, PartialEq)]
pub enum Atom {
    // String
    Str(String),

    // Unsigned 64-bit integer
    UInt(u64),

    // Signed 64-bit integer
    SInt(i64),

    // 64-bit floating point number
    Float(f64),
}

// Str
impl From<String> for Atom {
    fn from(s: String) -> Atom {
        Atom::Str(s)
    }
}

impl<'a> From<&'a str> for Atom {
    fn from(s: &'a str) -> Atom {
        Atom::Str(s.to_string())
    }
}

// Uint
impl From<usize> for Atom {
    fn from(u: usize) -> Atom {
        Atom::UInt(u as u64)
    }
}

impl From<u64> for Atom {
    fn from(u: u64) -> Atom {
        Atom::UInt(u.into())
    }
}
impl From<u32> for Atom {
    fn from(u: u32) -> Atom {
        Atom::UInt(u.into())
    }
}

impl From<u16> for Atom {
    fn from(u: u16) -> Atom {
        Atom::UInt(u.into())
    }
}

impl From<u8> for Atom {
    fn from(u: u8) -> Atom {
        Atom::UInt(u.into())
    }
}

// SInt
impl From<isize> for Atom {
    fn from(i: isize) -> Atom {
        Atom::SInt(i as i64)
    }
}

impl From<i64> for Atom {
    fn from(i: i64) -> Atom {
        Atom::SInt(i.into())
    }
}

impl From<i32> for Atom {
    fn from(i: i32) -> Atom {
        Atom::SInt(i.into())
    }
}

impl From<i16> for Atom {
    fn from(i: i16) -> Atom {
        Atom::SInt(i.into())
    }
}

impl From<i8> for Atom {
    fn from(i: i8) -> Atom {
        Atom::SInt(i.into())
    }
}

// Float
impl From<f64> for Atom {
    fn from(f: f64) -> Atom {
        Atom::Float(f.into())
    }
}

impl From<f32> for Atom {
    fn from(f: f32) -> Atom {
        Atom::Float(f.into())
    }
}


fn is_num_string(s: &str) -> bool {
    match s.slice_shift_char() {
        None => false,
        Some((c, rest)) => {
            if char::is_numeric(c) {
                true
            } else if c == '-' || c == '+' {
                match rest.slice_shift_char() {
                    Some((c, _)) if char::is_numeric(c) => true,
                    _ => false,
                }
            } else {
                false
            }
        }
    }
}

fn quote(s: &str) -> Cow<str> {
    if s.is_empty() {
        Cow::Borrowed("\"\"")
    } else if is_num_string(s) || s.contains(is_token_delim) {
        let mut r: String = "\"".to_string();
        r.push_str(&s.replace("\\", "\\\\").replace("\"", "\\\""));
        r.push_str("\"");
        Cow::Owned(r)
    } else {
        Cow::Borrowed(s)
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Atom::Str(ref s) => write!(f, "{}", quote(s)),
            Atom::UInt(u) => write!(f, "{}", u),
            Atom::SInt(s) => write!(f, "{}", s),
            Atom::Float(n) => write!(f, "{}", n),
        }
    }
}

#[test]
fn test_display() {
    assert_eq!("12345", &format!("{}", Atom::UInt(12345)));
    assert_eq!("12345", &format!("{}", Atom::SInt(12345)));
    assert_eq!("-12345", &format!("{}", Atom::SInt(-12345)));
    assert_eq!("-12345.5", &format!("{}", Atom::Float(-12345.5)));
    assert_eq!("abc", &format!("{}", Atom::Str("abc".to_string())));
    assert_eq!("\"(\"", &format!("{}", Atom::Str("(".to_string())));
    assert_eq!("\"\\\"\"", &format!("{}", Atom::Str("\"".to_string())));
    assert_eq!("\"123\"", &format!("{}", Atom::Str("123".to_string())));
    assert_eq!("\"123abc\"",
               &format!("{}", Atom::Str("123abc".to_string())));
    assert_eq!("\"+123\"", &format!("{}", Atom::Str("+123".to_string())));
    assert_eq!("+", &format!("{}", Atom::Str("+".to_string())));
    assert_eq!("-", &format!("{}", Atom::Str("-".to_string())));
    assert_eq!("-b", &format!("{}", Atom::Str("-b".to_string())));
    assert_eq!("abc123", &format!("{}", Atom::Str("abc123".to_string())));
}
