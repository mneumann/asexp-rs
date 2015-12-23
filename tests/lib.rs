extern crate asexp;

use asexp::Expr;
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
    let expr = Expr::parse(&s).unwrap();
    let d = format!("{}\n", expr);
    assert_eq!(s, d);
}
