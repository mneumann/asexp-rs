use super::token::Token;
use super::Atom;
use super::Sexp;

#[cfg(test)]
use super::token::Tokenizer;

#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedToken(Token<'a>),
    UnexpectedEnd,
}

// Returns either the successfully parsed expression, or
// the invalid token which was not expected.
pub fn parse_sexp<'a, I>(tokenstream: &mut I) -> Result<Sexp, ParseError<'a>>
where
    I: Iterator<Item = Token<'a>>,
{
    match tokenstream.next() {
        None => Err(ParseError::UnexpectedEnd),
        Some(tok) => match tok {
            Token::Str(s) => Ok(Sexp::from(s)),
            Token::QStr(s) => Ok(Sexp::from(s)),
            Token::UInt(u) => Ok(Sexp::from(Atom::UInt(u))),
            Token::SInt(s) => Ok(Sexp::from(Atom::SInt(s))),
            Token::Float(s) => Ok(Sexp::from(Atom::Float(s))),
            Token::OpenParens => {
                let mut exprs = vec![];
                loop {
                    match parse_sexp(tokenstream) {
                        Ok(ex) => exprs.push(ex),
                        Err(ParseError::UnexpectedToken(Token::CloseParens)) => break,
                        Err(err) => return Err(err),
                    }
                }
                Ok(Sexp::Tuple(exprs))
            }
            Token::OpenBracket => {
                let mut exprs = vec![];
                loop {
                    match parse_sexp(tokenstream) {
                        Ok(ex) => exprs.push(ex),
                        Err(ParseError::UnexpectedToken(Token::CloseBracket)) => break,
                        Err(err) => return Err(err),
                    }
                }
                Ok(Sexp::Array(exprs))
            }
            Token::OpenCurly => {
                let mut exprs = vec![];
                loop {
                    match parse_sexp(tokenstream) {
                        Ok(key) => match parse_sexp(tokenstream) {
                            Ok(value) => {
                                exprs.push((key, value));
                            }
                            Err(err) => return Err(err),
                        },
                        Err(ParseError::UnexpectedToken(Token::CloseCurly)) => break,
                        Err(err) => return Err(err),
                    }
                }
                Ok(Sexp::Map(exprs))
            }
            tok => Err(ParseError::UnexpectedToken(tok)),
        },
    }
}

pub fn at_end<'a, I>(tokenstream: &mut I) -> bool
where
    I: Iterator<Item = Token<'a>>,
{
    tokenstream.next().is_none()
}

#[test]
fn test_parse_sexp() {
    let mut p = Tokenizer::new("123.45", true);
    assert_eq!(Ok(Sexp::from(123.45)), parse_sexp(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("123", true);
    assert_eq!(Ok(Sexp::from(123usize)), parse_sexp(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("+123", true);
    assert_eq!(Ok(Sexp::from(123isize)), parse_sexp(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("\"hal\\\"lo\"", true);
    assert_eq!(Ok(Sexp::from("hal\"lo")), parse_sexp(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("(123)", true);
    assert_eq!(Ok(Sexp::from((123usize,))), parse_sexp(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("(test 123)   ", true);
    assert_eq!(Ok(Sexp::from(("test", 123usize))), parse_sexp(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("(test 123)   123", true);
    assert_eq!(Ok(Sexp::from(("test", 123usize))), parse_sexp(&mut p));
    assert_eq!(false, at_end(&mut p));

    let mut p = Tokenizer::new("   ", true);
    assert_eq!(Err(ParseError::UnexpectedEnd), parse_sexp(&mut p));

    let mut p = Tokenizer::new("(abc    ", true);
    assert_eq!(Err(ParseError::UnexpectedEnd), parse_sexp(&mut p));

    let mut p = Tokenizer::new("(abc  }  ", true);
    assert_eq!(
        Err(ParseError::UnexpectedToken(Token::CloseCurly)),
        parse_sexp(&mut p)
    );

    let mut p = Tokenizer::new("[1 2 3]", true);
    assert_eq!(
        Ok(Sexp::Array(vec![
            Sexp::from(1usize),
            Sexp::from(2usize),
            Sexp::from(3usize)
        ])),
        parse_sexp(&mut p)
    );
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("{a 1 b 2}", true);
    assert_eq!(
        Ok(Sexp::Map(vec![
            (Sexp::from("a"), Sexp::from(1usize)),
            (Sexp::from("b"), Sexp::from(2usize))
        ])),
        parse_sexp(&mut p)
    );
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("{a 1 b }", true);
    assert_eq!(
        Err(ParseError::UnexpectedToken(Token::CloseCurly)),
        parse_sexp(&mut p)
    );
}
