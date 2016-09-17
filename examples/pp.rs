extern crate asexp;
extern crate grabinput;

use asexp::Sexp;
use asexp::sexp::prettyprint;

fn main() {
    let s = grabinput::from_args().with_fallback().all();
    let expr = Sexp::parse_toplevel(&s).unwrap();
    let _ = prettyprint(&expr, &mut std::io::stdout(), 0, false).unwrap();
}
