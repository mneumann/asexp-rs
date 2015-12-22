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
