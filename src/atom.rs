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
impl From<String> for Atom
{
    fn from(s: String) -> Atom {
        Atom::Str(s)
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
