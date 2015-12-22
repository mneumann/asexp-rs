#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Error,
    Whitespace(&'a str),

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

#[inline]
fn scan<F: Fn(char) -> bool>(s: &str, cond: F) -> (&str, &str) {
    // split at the first non-"cond" character
    match s.find(|c: char| !cond(c)) {
        None => (s, ""),
        Some(pos) => s.split_at(pos),
    }
}


#[inline]
fn is_token_delim(c: char) -> bool {
    c.is_whitespace() || c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' ||
    c == '"'
}

// TODO ; comment
fn next_token<'a>(s: &'a str) -> Option<(Token<'a>, &'a str)> {
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

                    let (basis, rest) = scan(s, char::is_numeric);
                    match rest.slice_shift_char() {
                        Some(('.', mantisse)) => {
                            // integer followed by "." => float
                            let (mantisse, _) = scan(mantisse, char::is_numeric);
                            let (full_float, rest) = s.split_at(basis.len() + 1 + mantisse.len());

                            Some((Token::Float(full_float), rest))
                        }
                        _ => Some((Token::Int(basis), rest)),
                    }
                }

                c if char::is_whitespace(c) => {
                    let (ws, rest) = scan(s, char::is_whitespace);
                    Some((Token::Whitespace(ws), rest))
                }

                _ => {
                    // Str
                    let (string, rest) = scan(s, |c| !is_token_delim(c));
                    Some((Token::Str(string), rest))
                }
            }
        }
    }
}

pub struct Tokenizer<'a> {
    current: &'a str,
    ignore_ws: bool,
}

impl<'a> Tokenizer<'a> {
    fn new(s: &'a str, ignore_ws: bool) -> Tokenizer<'a> {
        Tokenizer {
            current: s,
            ignore_ws: ignore_ws,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match next_token(self.current) {
                Some((tok, rest)) => {
                    self.current = rest;
                    if self.ignore_ws {
                        if let Token::Whitespace(_) = tok {
                            continue;
                        }
                    }
                    return Some(tok);
                }
                None => {
                    return None;
                }
            }
        }
    }
}


#[test]
fn test_tokenizer_whitespace() {
    let t = Tokenizer::new(" (abc 123)", false);
    let tokens: Vec<_> = t.into_iter().collect();
    assert_eq!(vec![Token::Whitespace(" "),
                    Token::OpenBrace,
                    Token::Str("abc"),
                    Token::Whitespace(" "),
                    Token::Int("123"),
                    Token::CloseBrace],
               tokens);
}

#[test]
fn test_tokenizer_no_whitespace() {
    let t = Tokenizer::new(" (abc 123)", true);
    let tokens: Vec<_> = t.into_iter().collect();
    assert_eq!(vec![Token::OpenBrace, Token::Str("abc"), Token::Int("123"), Token::CloseBrace],
               tokens);
}

#[test]
fn test_token() {
    assert_eq!(Some((Token::Whitespace("  "), "abc")), next_token("  abc"));
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
