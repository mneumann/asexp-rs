extern crate asexp;
extern crate grabinput;

use asexp::Sexp;
use asexp::sexp::prettyprint;

fn main() {
    let s = grabinput::all(std::env::args().nth(1));
    let expr = Sexp::parse_toplevel(&s).unwrap();
    let _ = prettyprint(&expr, &mut std::io::stdout(), 0, false).unwrap();
}
