extern crate asexp;

use asexp::Sexp;
use std::fs::File;
use std::io::Read;

fn read_file(fname: &str) -> String {
    let mut s = String::new();
    let _ = File::open(fname).unwrap().read_to_string(&mut s).unwrap();
    s
}

#[test]
fn node() {
    let s = read_file("tests/node.as");
    let expr = Sexp::parse(&s).unwrap();
    let d = format!("{}\n", expr);
    assert_eq!(s, d);
}
