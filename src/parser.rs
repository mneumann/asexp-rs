use super::Expr;
use super::Atom;
use super::token::{Token, Tokenizer};

pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
}

#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedToken(Token<'a>),
    UnexpectedEnd,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Parser<'a> {
        Parser { tokenizer: Tokenizer::new(s, true) }
    }

    pub fn at_end(&mut self) -> bool {
        self.tokenizer.next().is_none()
    }

    // Returns either the successfully parsed expression, or
    // the invalid token which was not expected.
    pub fn parse_expr(&mut self) -> Result<Expr, ParseError<'a>> {
        match self.tokenizer.next() {
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
                            match self.parse_expr() {
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
                            match self.parse_expr() {
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
                            match self.parse_expr() {
                                Ok(key) => {
                                    match self.parse_expr() {
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
}

#[test]
fn test_parse_expr() {
    let mut p = Parser::new("123.45");
    assert_eq!(Ok(Expr::from(123.45)), p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("123");
    assert_eq!(Ok(Expr::from(123usize)), p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("+123");
    assert_eq!(Ok(Expr::from(123isize)), p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("\"hal\\\"lo\"");
    assert_eq!(Ok(Expr::from("hal\"lo")), p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("(123)");
    assert_eq!(Ok(Expr::from((123usize,))), p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("(test 123)   ");
    assert_eq!(Ok(Expr::from(("test", 123usize))), p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("(test 123)   123");
    assert_eq!(Ok(Expr::from(("test", 123usize))), p.parse_expr());
    assert_eq!(false, p.at_end());

    let mut p = Parser::new("   ");
    assert_eq!(Err(ParseError::UnexpectedEnd), p.parse_expr());

    let mut p = Parser::new("(abc    ");
    assert_eq!(Err(ParseError::UnexpectedEnd), p.parse_expr());

    let mut p = Parser::new("(abc  }  ");
    assert_eq!(Err(ParseError::UnexpectedToken(Token::CloseCurly)),
               p.parse_expr());

    let mut p = Parser::new("[1 2 3]");
    assert_eq!(Ok(Expr::Array(vec![Expr::from(1usize), Expr::from(2usize), Expr::from(3usize)])),
               p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("{a 1 b 2}");
    assert_eq!(Ok(Expr::Map(vec![(Expr::from("a"), Expr::from(1usize)),
                                 (Expr::from("b"), Expr::from(2usize))])),
               p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("{a 1 b }");
    assert_eq!(Err(ParseError::UnexpectedToken(Token::CloseCurly)),
               p.parse_expr());
}
