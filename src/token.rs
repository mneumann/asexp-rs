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

    Str(&'a str),
    QStr(String),

    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    OpenCurly,
    CloseCurly,

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
    c.is_whitespace() || c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}'
}

fn is_valid_unquoted_string(s: &str) -> bool {
    !(s.contains('\\') || s.contains('"'))
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
                    let mut rest = s2;
                    let mut unquoted_string = String::new();

                    loop {
                        match rest.slice_shift_char() {
                            None => {
                                // Error. No terminating quoting character found
                                return Some((Token::Error((rest, TokenError::MissingQuoteEnd)), s));
                            }
                            Some((ch, rem)) => {
                                match ch {
                                    '"' => {
                                        rest = rem;
                                        // found good string
                                        break;
                                    }
                                    '\\' => {
                                        // next character is escaped
                                        rest = rem;
                                        match rest.slice_shift_char() {
                                            Some(('\\', r)) => {
                                                rest = r;
                                                unquoted_string.push('\\');
                                            }
                                            Some(('"', r)) => {
                                                rest = r;
                                                unquoted_string.push('"');
                                            }
                                            _ => {
                                                return Some((Token::Error((rest, TokenError::InvalidEscape)), s));
                                            }
                                        }
                                    }
                                    _ => {
                                        rest = rem;
                                        unquoted_string.push(ch);
                                    }
                                }
                            }
                        }
                    }

                    Some((Token::QStr(unquoted_string), rest))
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
                    match s2.slice_shift_char() {
                        Some((ch, _)) if char::is_digit(ch, 10) => {
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
                                Some((Token::Error((string, TokenError::InvalidUnquotedString)),
                                      rest))
                            }
                        }
                    }
                }

                '0'...'9' => {
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
                        Some((Token::Error((string, TokenError::InvalidUnquotedString)),
                              rest))
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
                    Token::UInt(123),
                    Token::CloseBrace],
               tokens);
}

#[test]
fn test_tokenizer_no_whitespace() {
    let t = Tokenizer::new(" (abc 123)", true);
    let tokens: Vec<_> = t.into_iter().collect();
    assert_eq!(vec![Token::OpenBrace, Token::Str("abc"), Token::UInt(123), Token::CloseBrace],
               tokens);
}

#[test]
fn test_token() {
    assert_eq!(Some((Token::Whitespace("  "), "abc")), next_token("  abc"));
    assert_eq!(Some((Token::Str("abc"), "")), next_token("abc"));
    assert_eq!(Some((Token::Str("abc"), "(")), next_token("abc("));

    assert_eq!(Some((Token::OpenBrace, ")")), next_token("()"));

    assert_eq!(Some((Token::UInt(12345), "")), next_token("12345"));
    assert_eq!(Some((Token::UInt(12345), " ")), next_token("12345 "));
    assert_eq!(Some((Token::SInt(12345), " ")), next_token("+12345 "));
    assert_eq!(Some((Token::SInt(-12345), " ")), next_token("-12345 "));
    assert_eq!(Some((Token::Str("-a"), " ")), next_token("-a "));
    assert_eq!(Some((Token::Str("+a"), " ")), next_token("+a "));
    assert_eq!(Some((Token::Str("+a"), "(")), next_token("+a("));

    assert_eq!(Some((Token::Error(("12345+", TokenError::InvalidNumber)), "")),
               next_token("12345+"));
    assert_eq!(Some((Token::UInt(12345), " +")), next_token("12345 +"));
    assert_eq!(Some((Token::Float(12345.123), "")), next_token("12345.123"));
    assert_eq!(Some((Token::Float(12345.123), "(")),
               next_token("12345.123("));

    assert_eq!(Some((Token::Error(("abc\\", TokenError::InvalidUnquotedString)),
                     " test")),
               next_token("abc\\ test"));
    assert_eq!(Some((Token::Error(("abc\"", TokenError::InvalidUnquotedString)),
                     " test")),
               next_token("abc\" test"));


    assert_eq!(Some((Token::QStr("".to_string()), "(")),
               next_token("\"\"("));
    assert_eq!(Some((Token::QStr("abc".to_string()), "(")),
               next_token("\"abc\"("));
    assert_eq!(Some((Token::QStr("a\"b".to_string()), "(")),
               next_token("\"a\\\"b\"("));
    assert_eq!(Some((Token::QStr("a\\b".to_string()), "(")),
               next_token("\"a\\\\b\"("));

    assert_eq!(Some((Token::Error(("", TokenError::MissingQuoteEnd)), "\"abc ")),
               next_token("\"abc "));

    assert_eq!(Some((Token::Error(("n ", TokenError::InvalidEscape)), "\"abc\\n ")),
               next_token("\"abc\\n "));


}
