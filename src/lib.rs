#![feature(str_char)]

pub mod atom;
pub mod expr;
mod token;

pub use atom::Atom;
pub use expr::Expr;

#[derive(Debug, PartialEq)]
enum ParseError {
    PrematureEOS,
}

// impl Atom {
// fn from_str(s: &str) -> Result<(Atom, &str), ParseError> {
// let delim: &[_] = &[' ', '\n', '\t', '(', ')', '[', ']', '{', '}'];
// match s.find(delim) {
// Some(pos) => Ok((Atom::Str(s[..pos].to_string()), &s[pos..])),
// None => {
// complete string
// Ok((Atom::Str(s.to_string()), ""))
// }
// }
// }
// }
//
