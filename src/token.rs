#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenError {
    // If a token not a valid number
    // For example "+0a" starts as a number, but contains invalid characters.
    InvalidNumber,

    // missing terminating quote "
    MissingQuoteEnd,

    InvalidUnquotedString,

    InvalidEscape,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Error((&'a str, TokenError)),
    Whitespace(&'a str),
    Comment(&'a str),

    Str(&'a str),
    QStr(String),

    OpenBracket,  // '['
    CloseBracket, // ']'
    OpenParens,   // '('
    CloseParens,  // ')'
    OpenCurly,    // '{'
    CloseCurly,   // '}'

    UInt(u64),
    SInt(i64),
    Float(f64),
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
pub fn is_token_delim(c: char) -> bool {
    c.is_whitespace()
        || c == '('
        || c == ')'
        || c == '['
        || c == ']'
        || c == '{'
        || c == '}'
        || c == '#'
}

fn is_valid_unquoted_string(s: &str) -> bool {
    !(s.contains('\\') || s.contains('"'))
}

fn next_token<'a>(s: &'a str) -> Option<(Token<'a>, &'a str)> {
    let mut chars = s.chars();
    match chars.next() {
        None => None,
        Some(c) => {
            match c {
                '(' => Some((Token::OpenParens, chars.as_str())),
                ')' => Some((Token::CloseParens, chars.as_str())),

                '[' => Some((Token::OpenBracket, chars.as_str())),
                ']' => Some((Token::CloseBracket, chars.as_str())),

                '{' => Some((Token::OpenCurly, chars.as_str())),
                '}' => Some((Token::CloseCurly, chars.as_str())),

                '#' => {
                    // comment
                    let (comment, rest) = scan(chars.as_str(), |ch| ch != '\n');
                    Some((Token::Comment(comment), rest))
                }

                '"' => {
                    let mut unquoted_string = String::new();

                    loop {
                        match chars.next() {
                            None => {
                                // Error. No terminating quoting character found
                                return Some((
                                    Token::Error((chars.as_str(), TokenError::MissingQuoteEnd)),
                                    s,
                                ));
                            }
                            Some(ch) => {
                                match ch {
                                    '"' => {
                                        // found good string
                                        break;
                                    }
                                    '\\' => {
                                        // next character is escaped
                                        match chars.next() {
                                            Some('\\') => {
                                                unquoted_string.push('\\');
                                            }
                                            Some('"') => {
                                                unquoted_string.push('"');
                                            }
                                            _ => {
                                                return Some((
                                                    Token::Error((
                                                        chars.as_str(),
                                                        TokenError::InvalidEscape,
                                                    )),
                                                    s,
                                                ));
                                            }
                                        }
                                    }
                                    _ => {
                                        unquoted_string.push(ch);
                                    }
                                }
                            }
                        }
                    }

                    Some((Token::QStr(unquoted_string), chars.as_str()))
                }

                c if char::is_whitespace(c) => {
                    let (ws, rest) = scan(s, char::is_whitespace);
                    Some((Token::Whitespace(ws), rest))
                }

                '+' | '-' => {
                    let (string, rest) = scan(s, |ch| !is_token_delim(ch));
                    assert!(string.len() > 0);

                    // parse +|- always as a signed integer first.
                    // this is the way to distinguish a positive signed
                    // integer from a positive unsigned (+1 == SInt, while 1 == UInt).

                    // if it is followed by a digit, this is should be a number.
                    match chars.next() {
                        Some(ch) if char::is_digit(ch, 10) => {
                            // +|- followed by [0-9]. This must be a number!

                            if let Ok(i) = string.parse::<i64>() {
                                Some((Token::SInt(i), rest))
                            } else if let Ok(i) = string.parse::<f64>() {
                                Some((Token::Float(i), rest))
                            } else {
                                Some((Token::Error((string, TokenError::InvalidNumber)), rest))
                            }
                        }
                        _ => {
                            // If it is followed by any other character (or none) it is a valid
                            // string token.
                            if is_valid_unquoted_string(string) {
                                Some((Token::Str(string), rest))
                            } else {
                                Some((
                                    Token::Error((string, TokenError::InvalidUnquotedString)),
                                    rest,
                                ))
                            }
                        }
                    }
                }

                '0'..='9' => {
                    // this should be either a unsigned integer of a floating point number. If not,
                    // it's invalid.
                    let (string, rest) = scan(s, |ch| !is_token_delim(ch));
                    assert!(string.len() > 0);

                    if let Ok(i) = string.parse::<u64>() {
                        Some((Token::UInt(i), rest))
                    } else if let Ok(i) = string.parse::<f64>() {
                        Some((Token::Float(i), rest))
                    } else {
                        Some((Token::Error((string, TokenError::InvalidNumber)), rest))
                    }
                }

                _ => {
                    // this neither starts with '+' or '-' or 'digit'. this is definitively a
                    // string.

                    let (string, rest) = scan(s, |ch| !is_token_delim(ch));
                    assert!(string.len() > 0);

                    if is_valid_unquoted_string(string) {
                        Some((Token::Str(string), rest))
                    } else {
                        Some((
                            Token::Error((string, TokenError::InvalidUnquotedString)),
                            rest,
                        ))
                    }
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
    pub fn new(s: &'a str, ignore_ws: bool) -> Tokenizer<'a> {
        Tokenizer {
            current: s,
            ignore_ws: ignore_ws,
        }
    }

    pub fn with_curly_around(self) -> CurlyAroundTokenizer<'a> {
        CurlyAroundTokenizer::new(self)
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
                        match tok {
                            Token::Whitespace(_) | Token::Comment(_) => continue,
                            _ => {}
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

enum State {
    Begin,
    Inner,
    End,
}

pub struct CurlyAroundTokenizer<'a> {
    inner: Tokenizer<'a>,
    state: State,
}

impl<'a> CurlyAroundTokenizer<'a> {
    pub fn new(inner: Tokenizer<'a>) -> CurlyAroundTokenizer<'a> {
        CurlyAroundTokenizer {
            inner: inner,
            state: State::Begin,
        }
    }
}

impl<'a> Iterator for CurlyAroundTokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            State::Begin => {
                self.state = State::Inner;
                Some(Token::OpenCurly)
            }
            State::Inner => {
                if let Some(tok) = self.inner.next() {
                    return Some(tok);
                }
                self.state = State::End;
                Some(Token::CloseCurly)
            }
            State::End => None,
        }
    }
}

#[test]
fn test_tokenizer_whitespace() {
    let t = Tokenizer::new(" (abc 123)", false);
    let tokens: Vec<_> = t.into_iter().collect();
    assert_eq!(
        vec![
            Token::Whitespace(" "),
            Token::OpenParens,
            Token::Str("abc"),
            Token::Whitespace(" "),
            Token::UInt(123),
            Token::CloseParens
        ],
        tokens
    );
}

#[test]
fn test_tokenizer_comment() {
    let t = Tokenizer::new(" (abc#comment\n 123)", false);
    let tokens: Vec<_> = t.into_iter().collect();
    assert_eq!(
        vec![
            Token::Whitespace(" "),
            Token::OpenParens,
            Token::Str("abc"),
            Token::Comment("comment"),
            Token::Whitespace("\n "),
            Token::UInt(123),
            Token::CloseParens
        ],
        tokens
    );
}

#[test]
fn test_tokenizer_curly_around() {
    let t = CurlyAroundTokenizer::new(Tokenizer::new(" (abc 123)", true));
    let tokens: Vec<_> = t.into_iter().collect();
    assert_eq!(
        vec![
            Token::OpenCurly,
            Token::OpenParens,
            Token::Str("abc"),
            Token::UInt(123),
            Token::CloseParens,
            Token::CloseCurly
        ],
        tokens
    );
}

#[test]
fn test_tokenizer_no_whitespace() {
    let t = Tokenizer::new(" (abc 123)", true);
    let tokens: Vec<_> = t.into_iter().collect();
    assert_eq!(
        vec![
            Token::OpenParens,
            Token::Str("abc"),
            Token::UInt(123),
            Token::CloseParens
        ],
        tokens
    );
}

#[test]
fn test_token() {
    assert_eq!(Some((Token::Whitespace("  "), "abc")), next_token("  abc"));
    assert_eq!(Some((Token::Str("abc"), "")), next_token("abc"));
    assert_eq!(Some((Token::Str("abc"), "(")), next_token("abc("));

    assert_eq!(Some((Token::OpenParens, ")")), next_token("()"));

    assert_eq!(Some((Token::UInt(12345), "")), next_token("12345"));
    assert_eq!(Some((Token::UInt(12345), " ")), next_token("12345 "));
    assert_eq!(Some((Token::SInt(12345), " ")), next_token("+12345 "));
    assert_eq!(Some((Token::SInt(-12345), " ")), next_token("-12345 "));
    assert_eq!(Some((Token::Str("-a"), " ")), next_token("-a "));
    assert_eq!(Some((Token::Str("+a"), " ")), next_token("+a "));
    assert_eq!(Some((Token::Str("+a"), "(")), next_token("+a("));

    assert_eq!(
        Some((Token::Error(("12345+", TokenError::InvalidNumber)), "")),
        next_token("12345+")
    );
    assert_eq!(Some((Token::UInt(12345), " +")), next_token("12345 +"));
    assert_eq!(Some((Token::Float(12345.123), "")), next_token("12345.123"));
    assert_eq!(
        Some((Token::Float(12345.123), "(")),
        next_token("12345.123(")
    );

    assert_eq!(
        Some((
            Token::Error(("abc\\", TokenError::InvalidUnquotedString)),
            " test"
        )),
        next_token("abc\\ test")
    );
    assert_eq!(
        Some((
            Token::Error(("abc\"", TokenError::InvalidUnquotedString)),
            " test"
        )),
        next_token("abc\" test")
    );

    assert_eq!(
        Some((Token::QStr("".to_string()), "(")),
        next_token("\"\"(")
    );
    assert_eq!(
        Some((Token::QStr("abc".to_string()), "(")),
        next_token("\"abc\"(")
    );
    assert_eq!(
        Some((Token::QStr("a\"b".to_string()), "(")),
        next_token("\"a\\\"b\"(")
    );
    assert_eq!(
        Some((Token::QStr("a\\b".to_string()), "(")),
        next_token("\"a\\\\b\"(")
    );

    assert_eq!(
        Some((Token::Error(("", TokenError::MissingQuoteEnd)), "\"abc ")),
        next_token("\"abc ")
    );

    //assert_eq!(Some((Token::Error(("n ", TokenError::InvalidEscape)), "\"abc\\n ")),
    assert_eq!(
        Some((Token::Error((" ", TokenError::InvalidEscape)), "\"abc\\n ")),
        next_token("\"abc\\n ")
    );
}
