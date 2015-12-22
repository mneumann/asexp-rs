use super::atom::Atom;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Atom(Atom),

    // ( ... )
    Tuple(Vec<Atom>),

    // [ ... ]
    Array(Vec<Atom>),

    // { key val ... }
    Map(Vec<(Atom, Atom)>),
}
