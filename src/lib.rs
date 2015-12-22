#![feature(str_char)]

pub mod atom;
mod token;

use std::char;
pub use atom::Atom;


#[derive(Debug, PartialEq)]
enum Expr {
    Atom(Atom),

    // ( ... )
    Tuple(Vec<Atom>),

    // [ ... ]
    Array(Vec<Atom>),

    // { key val ... }
    Map(Vec<(Atom, Atom)>),
}

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
