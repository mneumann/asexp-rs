#![feature(str_char)]

pub mod atom;
pub mod sexp;
pub mod parser;
pub mod token;

pub use atom::Atom;
pub use sexp::Sexp;

/// Convert self into a S-expression.
pub trait ToSexp {
    fn to_sexp(&self) -> Sexp;
}
