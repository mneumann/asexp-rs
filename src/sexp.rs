use std::fmt;
use super::atom::Atom;
use super::parser;
use super::token::{Token, Tokenizer, CurlyAroundTokenizer};
use std::collections::BTreeMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Sexp {
    Atom(Atom),

    // ( ... )
    Tuple(Vec<Sexp>),

    // [ ... ]
    Array(Vec<Sexp>),

    // { key val ... }
    Map(Vec<(Sexp, Sexp)>),
}

impl Sexp {
    /// Converts a Sexp::Map into a BTreeMap<String, Sexp> if possible.
    pub fn into_map(self) -> Result<BTreeMap<String, Sexp>, &'static str> {
        match self {
            Sexp::Map(pairs) => {
                let mut map = BTreeMap::new();
                for (key, val) in pairs {
                    match key {
                        Sexp::Atom(Atom::Str(s)) => {
                            if map.insert(s, val).is_some() {
                                return Err("duplicate key");
                            }
                        }
                        _ => return Err("key has to be a string"),
                    }
                }
                return Ok(map);
            }
            _ => Err("expr is not a Sexp::Map"),
        }
    }

    pub fn get_uint(&self) -> Option<u64> {
        match self {
            &Sexp::Atom(Atom::UInt(u)) => Some(u),
            _ => None,
        }
    }

    pub fn get_int(&self) -> Option<i64> {
        match self {
            &Sexp::Atom(Atom::SInt(s)) => Some(s),
            _ => None,
        }
    }

    pub fn get_float(&self) -> Option<f64> {
        match self {
            &Sexp::Atom(Atom::Float(f)) => Some(f),
            _ => None,
        }
    }

    pub fn get_str(&self) -> Option<&str> {
        match self {
            &Sexp::Atom(Atom::Str(ref s)) => Some(s),
            _ => None,
        }
    }

    pub fn get_vec<'a, F, R>(&'a self, f: F) -> Option<Vec<R>>
        where F: Fn(&'a Sexp) -> Option<R>
    {
        match self {
            &Sexp::Array(ref ary) => {
                let mut a = Vec::new();
                for elm in ary.iter() {
                    if let Some(u) = f(elm) {
                        a.push(u);
                    } else {
                        return None;
                    }
                }
                Some(a)
            }
            _ => None,
        }
    }

    pub fn get_uint_vec(&self) -> Option<Vec<u64>> {
        self.get_vec(|elm| elm.get_uint())
    }

    pub fn parse_iter<'a, I>(mut iter: I) -> Result<Sexp, ()>
        where I: Iterator<Item = Token<'a>>
    {
        if let Ok(expr) = parser::parse_sexp(&mut iter) {
            if parser::at_end(&mut iter) {
                return Ok(expr);
            }
        }
        Err(())
    }

    pub fn parse(s: &str) -> Result<Sexp, ()> {
        Sexp::parse_iter(Tokenizer::new(s, true))
    }

    pub fn parse_toplevel(s: &str) -> Result<Sexp, ()> {
        Sexp::parse_iter(CurlyAroundTokenizer::new(Tokenizer::new(s, true)))
    }
}

impl<T> From<T> for Sexp where T: Into<Atom>
{
    fn from(t: T) -> Sexp {
        Sexp::Atom(t.into())
    }
}

impl From<()> for Sexp {
    fn from(_e: ()) -> Sexp {
        Sexp::Tuple(vec![])
    }
}

impl<A> From<(A,)> for Sexp where A: Into<Sexp>
{
    fn from(e: (A,)) -> Sexp {
        Sexp::Tuple(vec![e.0.into()])
    }
}

impl<A, B> From<(A, B)> for Sexp
    where A: Into<Sexp>,
          B: Into<Sexp>
{
    fn from(e: (A, B)) -> Sexp {
        Sexp::Tuple(vec![e.0.into(), e.1.into()])
    }
}

impl<A, B, C> From<(A, B, C)> for Sexp
    where A: Into<Sexp>,
          B: Into<Sexp>,
          C: Into<Sexp>
{
    fn from(e: (A, B, C)) -> Sexp {
        Sexp::Tuple(vec![e.0.into(), e.1.into(), e.2.into()])
    }
}

impl<A, B, C, D> From<(A, B, C, D)> for Sexp
    where A: Into<Sexp>,
          B: Into<Sexp>,
          C: Into<Sexp>,
          D: Into<Sexp>
{
    fn from(e: (A, B, C, D)) -> Sexp {
        Sexp::Tuple(vec![e.0.into(), e.1.into(), e.2.into(), e.3.into()])
    }
}

impl<A> From<Vec<A>> for Sexp where A: Into<Sexp>
{
    fn from(arr: Vec<A>) -> Sexp {
        Sexp::Array(arr.into_iter().map(|e| e.into()).collect())
    }
}

impl fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Sexp::Atom(ref a) => write!(f, "{}", a),
            Sexp::Tuple(ref t) => {
                try!(write!(f, "("));
                for (i, expr) in t.iter().enumerate() {
                    let space = if i > 0 {
                        " "
                    } else {
                        ""
                    };
                    try!(write!(f, "{}{}", space, expr));
                }
                write!(f, ")")
            }
            Sexp::Array(ref t) => {
                try!(write!(f, "["));
                for (i, expr) in t.iter().enumerate() {
                    let space = if i > 0 {
                        " "
                    } else {
                        ""
                    };
                    try!(write!(f, "{}{}", space, expr));
                }
                write!(f, "]")
            }
            Sexp::Map(ref t) => {
                try!(write!(f, "{}", "{"));
                for (i, expr) in t.iter().enumerate() {
                    let space = if i > 0 {
                        " "
                    } else {
                        ""
                    };
                    try!(write!(f, "{}{} {}", space, expr.0, expr.1));
                }
                write!(f, "{}", "}")
            }

        }
    }
}


#[test]
fn test_display() {
    assert_eq!("12345", &format!("{}", Sexp::Atom(Atom::UInt(12345))));
    assert_eq!("1.0", &format!("{}", Sexp::Atom(Atom::Float(1.0))));

    assert_eq!("(12345)",
               &format!("{}", Sexp::Tuple(vec![Sexp::Atom(Atom::UInt(12345))])));
    assert_eq!("(12345 abc)",
               &format!("{}",
                        Sexp::Tuple(vec![Sexp::Atom(Atom::UInt(12345)),
                                         Sexp::Atom(Atom::Str("abc".to_string()))])));
    assert_eq!("(12345 (abc))",
               &format!("{}",
                        Sexp::Tuple(vec![Sexp::Atom(Atom::UInt(12345)),
                                         Sexp::Tuple(vec![
                                         Sexp::Atom(Atom::Str("abc".to_string()))
                                         ])])));

}

#[test]
fn test_from() {
    assert_eq!(Sexp::Atom(Atom::UInt(123)), Sexp::from(123u64));
    assert_eq!(Sexp::Atom(Atom::SInt(123)), Sexp::from(123i64));
    assert_eq!(Sexp::Atom(Atom::Str("test".to_string())),
               Sexp::from("test"));
    assert_eq!(Sexp::Atom(Atom::Float(123.45)), Sexp::from(123.45));
    assert_eq!(Sexp::Tuple(vec![Sexp::Atom(Atom::Float(123.45))]),
               Sexp::from((123.45,)));
    assert_eq!(Sexp::Array(vec![Sexp::Atom(Atom::Float(123.45))]),
               Sexp::from(vec![123.45]));

}

#[test]
fn test_parse() {
    assert_eq!(Ok(Sexp::from(("abc", 123u64, ("-", 123.43, 11.0)))),
               Sexp::parse("(abc 123 (- 123.43 11.0))"));
}

#[test]
fn test_parse_toplevel() {
    assert_eq!(Sexp::parse("{a 1 b 2}"), Sexp::parse_toplevel("a 1 b 2"));
}

#[test]
fn test_map() {
    let map = Sexp::parse("{a 1 b 2}").unwrap().into_map().unwrap();
    assert_eq!(true, map.contains_key("a"));
    assert_eq!(true, map.contains_key("b"));
    assert_eq!(false, map.contains_key("c"));

    assert_eq!(Err("duplicate key"),
               Sexp::parse("{a 1 b 2 a 1}").unwrap().into_map());
    assert_eq!(Err("key has to be a string"),
               Sexp::parse("{1 1 b 2 a 1}").unwrap().into_map());

}
