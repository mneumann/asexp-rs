use std::fmt;
use super::atom::Atom;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Atom(Atom),

    // ( ... )
    Tuple(Vec<Expr>), /* [ ... ]
                       * Array(Vec<Atom>),
                       *
                       * { key val ... }
                       * Map(Vec<(Atom, Atom)>),
                       * */
}

impl<T> From<T> for Expr where T: Into<Atom>
{
    fn from(t: T) -> Expr {
        Expr::Atom(t.into())
    }
}

impl From<()> for Expr {
    fn from(_e: ()) -> Expr {
        Expr::Tuple(vec![])
    }
}

impl<A> From<(A,)> for Expr where A: Into<Expr>
{
    fn from(e: (A,)) -> Expr {
        Expr::Tuple(vec![e.0.into()])
    }
}

impl<A, B> From<(A, B)> for Expr
    where A: Into<Expr>,
          B: Into<Expr>
{
    fn from(e: (A, B)) -> Expr {
        Expr::Tuple(vec![e.0.into(), e.1.into()])
    }
}

impl<A, B, C> From<(A, B, C)> for Expr
    where A: Into<Expr>,
          B: Into<Expr>,
          C: Into<Expr>
{
    fn from(e: (A, B, C)) -> Expr {
        Expr::Tuple(vec![e.0.into(), e.1.into(), e.2.into()])
    }
}

impl<A, B, C, D> From<(A, B, C, D)> for Expr
    where A: Into<Expr>,
          B: Into<Expr>,
          C: Into<Expr>,
          D: Into<Expr>
{
    fn from(e: (A, B, C, D)) -> Expr {
        Expr::Tuple(vec![e.0.into(), e.1.into(), e.2.into(), e.3.into()])
    }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Expr::Atom(ref a) => write!(f, "{}", a),
            Expr::Tuple(ref t) => {
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
        }
    }
}


#[test]
fn test_display() {
    assert_eq!("12345", &format!("{}", Expr::Atom(Atom::UInt(12345))));
    assert_eq!("(12345)",
               &format!("{}", Expr::Tuple(vec![Expr::Atom(Atom::UInt(12345))])));
    assert_eq!("(12345 abc)",
               &format!("{}",
                        Expr::Tuple(vec![Expr::Atom(Atom::UInt(12345)),
                                         Expr::Atom(Atom::Str("abc".to_string()))])));
    assert_eq!("(12345 (abc))",
               &format!("{}",
                        Expr::Tuple(vec![Expr::Atom(Atom::UInt(12345)),
                                         Expr::Tuple(vec![
                                         Expr::Atom(Atom::Str("abc".to_string()))
                                         ])])));

}

#[test]
fn test_from() {
    assert_eq!(Expr::Atom(Atom::UInt(123)), Expr::from(123u64));
    assert_eq!(Expr::Atom(Atom::SInt(123)), Expr::from(123i64));
    assert_eq!(Expr::Atom(Atom::Str("test".to_string())),
               Expr::from("test"));
    assert_eq!(Expr::Atom(Atom::Float(123.45)), Expr::from(123.45));
    assert_eq!(Expr::Tuple(vec![Expr::Atom(Atom::Float(123.45))]),
               Expr::from((123.45,)));
}
