use asexp::sexp::prettyprint;
use asexp::Sexp;

fn main() {
    let s = grabinput::all(std::env::args().nth(1));
    let expr = Sexp::parse_toplevel(&s).expect("valid Sexp");
    let _ = prettyprint(&expr, &mut std::io::stdout(), 0, false).expect("pretty");
}
