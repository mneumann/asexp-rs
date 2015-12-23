use super::Expr;
use super::Atom;
use super::token::{Token, Tokenizer};

pub struct Parser<'a> {
    current_token: Option<Token<'a>>,
    tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Parser<'a> {
        Parser {
            current_token: None,
            tokenizer: Tokenizer::new(s, true),
        }
    }

    // skips the current token
    fn skip(&mut self) {
        assert!(self.current_token.is_some());
        self.current_token = None;
    }

    // returns the current token.
    fn token(&mut self) -> Option<Token<'a>> {
        if let Some(token) = self.current_token {
            return Some(token);
        }
        assert!(self.current_token.is_none());
        self.current_token = self.tokenizer.next();
        self.current_token
    }

    pub fn at_end(&mut self) -> bool {
        self.token().is_none()
    }

    // Returns either the successfully parsed expression, or
    // the invalid token which was not expected.
    pub fn parse_expr(&mut self) -> Result<Expr, Token<'a>> {
        let current = self.token().unwrap();
        match current {
            Token::Str(s) => {
                self.skip();
                Ok(Expr::from(s))
            }
            Token::UInt(u) => {
                self.skip();
                Ok(Expr::from(Atom::UInt(u)))
            }
            Token::SInt(s) => {
                self.skip();
                Ok(Expr::from(Atom::SInt(s)))
            }
            Token::Float(s) => {
                self.skip();
                Ok(Expr::from(Atom::Float(s)))
            }
            Token::OpenBrace => {
                self.skip();
                let mut exprs = vec![];
                loop {
                    if let Some(Token::CloseBrace) = self.token() {
                        self.skip();
                        break;
                    }
                    match self.parse_expr() {
                        Ok(ex) => exprs.push(ex),
                        Err(tok) => return Err(tok),
                    }
                }
                Ok(Expr::Tuple(exprs))
            }
            tok => Err(tok),
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

    let mut p = Parser::new("(123)");
    assert_eq!(Ok(Expr::from((123usize,))), p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("(test 123)   ");
    assert_eq!(Ok(Expr::from(("test", 123usize))), p.parse_expr());
    assert_eq!(true, p.at_end());

    let mut p = Parser::new("(test 123)   123");
    assert_eq!(Ok(Expr::from(("test", 123usize))), p.parse_expr());
    assert_eq!(false, p.at_end());
}
