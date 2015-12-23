use super::Expr;
use super::Atom;
use super::token::Token;

#[cfg(test)]
use super::token::Tokenizer;

#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedToken(Token<'a>),
    UnexpectedEnd,
}

// Returns either the successfully parsed expression, or
// the invalid token which was not expected.
pub fn parse_expr<'a, I>(tokenstream: &mut I) -> Result<Expr, ParseError<'a>>
    where I: Iterator<Item = Token<'a>>
{
    match tokenstream.next() {
        None => Err(ParseError::UnexpectedEnd),
        Some(tok) => {
            match tok {
                Token::Str(s) => Ok(Expr::from(s)),
                Token::QStr(s) => Ok(Expr::from(s)),
                Token::UInt(u) => Ok(Expr::from(Atom::UInt(u))),
                Token::SInt(s) => Ok(Expr::from(Atom::SInt(s))),
                Token::Float(s) => Ok(Expr::from(Atom::Float(s))),
                Token::OpenBrace => {
                    let mut exprs = vec![];
                    loop {
                        match parse_expr(tokenstream) {
                            Ok(ex) => exprs.push(ex),
                            Err(ParseError::UnexpectedToken(Token::CloseBrace)) => break,
                            Err(err) => return Err(err),
                        }
                    }
                    Ok(Expr::Tuple(exprs))
                }
                Token::OpenBracket => {
                    let mut exprs = vec![];
                    loop {
                        match parse_expr(tokenstream) {
                            Ok(ex) => exprs.push(ex),
                            Err(ParseError::UnexpectedToken(Token::CloseBracket)) => break,
                            Err(err) => return Err(err),
                        }
                    }
                    Ok(Expr::Array(exprs))
                }
                Token::OpenCurly => {
                    let mut exprs = vec![];
                    loop {
                        match parse_expr(tokenstream) {
                            Ok(key) => {
                                match parse_expr(tokenstream) {
                                    Ok(value) => {
                                        exprs.push((key, value));
                                    }
                                    Err(err) => return Err(err),
                                }
                            }
                            Err(ParseError::UnexpectedToken(Token::CloseCurly)) => break,
                            Err(err) => return Err(err),
                        }
                    }
                    Ok(Expr::Map(exprs))
                }
                tok => Err(ParseError::UnexpectedToken(tok)),
            }
        }
    }
}

pub fn at_end<'a, I>(tokenstream: &mut I) -> bool
    where I: Iterator<Item = Token<'a>>
{
    tokenstream.next().is_none()
}

#[test]
fn test_parse_expr() {
    let mut p = Tokenizer::new("123.45", true);
    assert_eq!(Ok(Expr::from(123.45)), parse_expr(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("123", true);
    assert_eq!(Ok(Expr::from(123usize)), parse_expr(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("+123", true);
    assert_eq!(Ok(Expr::from(123isize)), parse_expr(&mut p));
    assert_eq!(true, at_end(&mut p));







    let mut p = Tokenizer::new("\"hal\\\"lo\"", true);
    assert_eq!(Ok(Expr::from("hal\"lo")), parse_expr(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("(123)", true);
    assert_eq!(Ok(Expr::from((123usize,))), parse_expr(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("(test 123)   ", true);
    assert_eq!(Ok(Expr::from(("test", 123usize))), parse_expr(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("(test 123)   123", true);
    assert_eq!(Ok(Expr::from(("test", 123usize))), parse_expr(&mut p));
    assert_eq!(false, at_end(&mut p));

    let mut p = Tokenizer::new("   ", true);
    assert_eq!(Err(ParseError::UnexpectedEnd), parse_expr(&mut p));

    let mut p = Tokenizer::new("(abc    ", true);
    assert_eq!(Err(ParseError::UnexpectedEnd), parse_expr(&mut p));

    let mut p = Tokenizer::new("(abc  }  ", true);
    assert_eq!(Err(ParseError::UnexpectedToken(Token::CloseCurly)),
               parse_expr(&mut p));

    let mut p = Tokenizer::new("[1 2 3]", true);
    assert_eq!(Ok(Expr::Array(vec![Expr::from(1usize), Expr::from(2usize), Expr::from(3usize)])),
               parse_expr(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("{a 1 b 2}", true);
    assert_eq!(Ok(Expr::Map(vec![(Expr::from("a"), Expr::from(1usize)),
                                 (Expr::from("b"), Expr::from(2usize))])),
               parse_expr(&mut p));
    assert_eq!(true, at_end(&mut p));

    let mut p = Tokenizer::new("{a 1 b }", true);
    assert_eq!(Err(ParseError::UnexpectedToken(Token::CloseCurly)),
               parse_expr(&mut p));
}
