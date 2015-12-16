#![feature(str_char)]

#[derive(Debug, PartialEq)]
enum Atom {
    // Unquoted string
    Str(String),

    // Quoted string
    QStr(String),

    // Unsigned 64-bit integer
    UInt(u64),

    // Signed 64-bit integer
    SInt(i64),

    // 64-bit floating point number
    Float(f64),
}

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

impl Atom {
    fn from_str(s: &str) -> Result<(Atom, &str), ParseError> {
        let delim: &[_] = &[' ', '\n', '\t', '(', ')', '[', ']', '{', '}'];
        match s.find(delim) {
            Some(pos) => Ok((Atom::Str(s[..pos].to_string()), &s[pos..])),
            None => {
                // complete string
                Ok((Atom::Str(s.to_string()), ""))
            }
        }
    }
}

#[derive(Debug, PartialEq)]
enum Token<'a> {
    Str(&'a str),
    QStr(&'a str),

    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    OpenCurly,
    CloseCurly,

    Int(&'a str),
    Float(&'a str),
}

const WHITESPACE: &'static [char] = &[' ', '\r', '\n', '\t'];
const TOKEN_DELIM: &'static [char] = &[' ', '\r', '\n', '\t', '(', ')', '[', ']', '{', '}', '"'];

fn skip_ws(s: &str) -> &str {
    s.trim_left_matches(WHITESPACE)
}

// ; comment
fn next_token<'a>(s: &'a str) -> Option<(Token<'a>, &'a str)> {
    let s = skip_ws(s);
    match s.slice_shift_char() {
        None => None,
        Some((c, s2)) => {
            match c {
                '(' => Some((Token::OpenBrace, s2)),
                ')' => Some((Token::CloseBrace, s2)),

                '[' => Some((Token::OpenBracket, s2)),
                ']' => Some((Token::CloseBracket, s2)),

                '{' => Some((Token::OpenCurly, s2)),
                '}' => Some((Token::CloseCurly, s2)),

                '"' => {
                    // TODO: QStr
                    None
                }

                '+' | '-' => {
                    // TODO
                    None
                }

                '0'...'9' => {
                    // Int, Float

                    match s.find(|c: char| !c.is_numeric()) {
                        Some(pos) => {
                            let (a, b) = s.split_at(pos);
                            match b.slice_shift_char() {
                                Some(('.', s2)) => {
                                    // integer continues
                                    match s2.find(|c: char| !c.is_numeric()) {
                                        None => Some((Token::Float(s), "")),
                                        Some(pos2) => {
                                            let (a, b) = s.split_at(pos + pos2 + 1);
                                            Some((Token::Float(a), b))
                                        }
                                    }
                                }
                                _ => Some((Token::Int(a), b)),
                            }
                        }
                        None => Some((Token::Int(s), "")),
                    }
                }

                _ => {
                    // Str
                    match s.find(TOKEN_DELIM) {
                        Some(pos) => {
                            let (a, b) = s.split_at(pos);
                            Some((Token::Str(a), b))
                        }
                        None => {
                            // complete string
                            Some((Token::Str(s), ""))
                        }
                    }
                }
            }
        }
    }
}

#[test]
fn test_tokenize() {
    assert_eq!(Some((Token::Str("abc"), "")), next_token("  abc"));
    assert_eq!(Some((Token::Str("abc"), "")), next_token("abc"));
    assert_eq!(Some((Token::Str("abc"), "(")), next_token("abc("));

    assert_eq!(Some((Token::OpenBrace, ")")), next_token("()"));

    assert_eq!(Some((Token::Int("12345"), "")), next_token("12345"));
    assert_eq!(Some((Token::Int("12345"), " ")), next_token("12345 "));
    assert_eq!(Some((Token::Int("12345"), "+")), next_token("12345+"));
    assert_eq!(Some((Token::Float("12345.123"), "")),
               next_token("12345.123"));
    assert_eq!(Some((Token::Float("12345.123"), "(")),
               next_token("12345.123("));
}
