use crate::lexer::{LexError, Lexer, Token};
use crate::location::{describe_location, Location};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Node {
    BinExpr {
        op: BinOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Num(i32),
}

enum ErrorKind<'a> {
    LexError(LexError<'a>),
    UnexpectedToken {
        got: Token<'a>,
        expected: Option<&'static str>,
    },
    UnexpectedEndOfFile {
        expected: Option<&'static str>,
    },
}

pub struct Error<'a> {
    kind: ErrorKind<'a>,
    location: Location,
    source: &'a str,
}

impl<'a> Error<'a> {
    fn new(kind: ErrorKind<'a>, location: Location, source: &'a str) -> Self {
        Self {
            kind,
            location,
            source,
        }
    }
}

impl<'a> std::fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ErrorKind::*;
        match &self.kind {
            LexError(e) => e.fmt(f)?,
            UnexpectedToken { got, expected } => match expected {
                Some(expected) => write!(f, "expected {} but got {}", expected, got)?,
                None => write!(f, "unexpected token {}", got)?,
            },
            UnexpectedEndOfFile { expected } => match expected {
                Some(expected) => write!(f, "expected {} but reached EOF", expected)?,
                None => write!(f, "reached EOF")?,
            },
        }
        describe_location(f, self.source, self.location)
    }
}

impl<'a> std::convert::From<LexError<'a>> for Error<'a> {
    fn from(v: LexError<'a>) -> Self {
        Self {
            location: v.location(),
            source: v.source(),
            kind: ErrorKind::LexError(v),
        }
    }
}

struct Tokens<'source> {
    lexer: Lexer<'source>,
    peek: Option<Result<(Token<'source>, Location), LexError<'source>>>,
}

impl<'source> Tokens<'source> {
    fn new(mut lexer: Lexer<'source>) -> Self {
        let peek = lexer.next();
        Self { lexer, peek }
    }

    fn peek(&self) -> Option<&Result<(Token<'source>, Location), LexError<'source>>> {
        self.peek.as_ref()
    }
}

impl<'source> std::iter::Iterator for Tokens<'source> {
    type Item = Result<(Token<'source>, Location), LexError<'source>>;

    fn next(&mut self) -> Option<Self::Item> {
        std::mem::replace(&mut self.peek, self.lexer.next())
    }
}

pub struct Parser<'source> {
    source: &'source str,
    tokens: Tokens<'source>,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Self {
        Parser {
            source: source,
            tokens: Tokens::new(Lexer::new(source)),
        }
    }

    fn error<T>(&self, kind: ErrorKind<'source>, location: Location) -> Result<T, Error<'source>> {
        Err(Error::new(kind, location, self.source))
    }

    fn unexpected_eof<T>(&self, expected: Option<&'static str>) -> Result<T, Error<'source>> {
        self.error(
            ErrorKind::UnexpectedEndOfFile { expected },
            Location::At(self.source.len()),
        )
    }

    fn unexpected_token<T>(
        &self,
        expected: &'static str,
        got: Token<'source>,
        location: Location,
    ) -> Result<T, Error<'source>> {
        self.error(
            ErrorKind::UnexpectedToken {
                got,
                expected: Some(expected),
            },
            location,
        )
    }

    fn peek(&self) -> Result<Option<(&Token, Location)>, Error<'source>> {
        match self.tokens.peek() {
            None => Ok(None),
            Some(Err(ref err)) => Err(err.clone().into()),
            Some(Ok((token, loc))) => Ok(Some((token, *loc))),
        }
    }

    fn next_token(
        &mut self,
        expected: &'static str,
    ) -> Result<(Token<'source>, Location), Error<'source>> {
        match self.tokens.next() {
            Some(x) => Ok(x?),
            None => self.unexpected_eof(Some(expected)),
        }
    }

    fn consume_reserved(
        &mut self,
        keyword: &'static str,
        expected: &'static str,
    ) -> Result<Option<(Token, Location)>, Error<'source>> {
        match self.peek()? {
            Some((Token::Reserved(s), _)) if s == &keyword => {
                let (token, loc) = self.next_token(expected)?;
                Ok(Some((token, loc)))
            }
            _ => Ok(None),
        }
    }

    fn expect_reserved(
        &mut self,
        keyword: &'static str,
        expected: &'static str,
    ) -> Result<(Token, Location), Error<'source>> {
        match self.next_token(expected)? {
            (Token::Reserved(s), loc) if &s == &keyword => Ok((Token::Reserved(s), loc)),
            (t, loc) => self.unexpected_token(expected, t, loc),
        }
    }

    fn parse_primary(&mut self) -> Result<Node, Error<'source>> {
        let (token, loc) = self.next_token("primary expression")?;
        match token {
            Token::Reserved("(") => {
                let expr = self.parse_expr()?;
                self.expect_reserved(")", "closing ')'")?;
                Ok(expr)
            }
            Token::Num(v) => Ok(Node::Num(v)),
            _ => self.unexpected_token("primary expression", token, loc),
        }
    }

    fn parse_unary(&mut self) -> Result<Node, Error<'source>> {
        if self.consume_reserved("+", "unary '+'")?.is_some() {
            return self.parse_primary();
        }
        if self.consume_reserved("-", "unary '-'")?.is_some() {
            return Ok(Node::BinExpr {
                op: BinOp::Sub,
                lhs: Box::new(Node::Num(0)),
                rhs: self.parse_primary().map(Box::new)?,
            });
        }
        self.parse_primary()
    }

    fn parse_mul(&mut self) -> Result<Node, Error<'source>> {
        let mut mul = self.parse_unary()?;
        loop {
            if self.consume_reserved("*", "operator '*'")?.is_some() {
                let rhs = self.parse_unary()?;
                mul = Node::BinExpr {
                    op: BinOp::Mul,
                    lhs: Box::new(mul),
                    rhs: Box::new(rhs),
                };
            } else if self.consume_reserved("/", "operator '/'")?.is_some() {
                let rhs = self.parse_unary()?;
                mul = Node::BinExpr {
                    op: BinOp::Div,
                    lhs: Box::new(mul),
                    rhs: Box::new(rhs),
                };
            } else {
                break;
            }
        }
        Ok(mul)
    }

    fn parse_add(&mut self) -> Result<Node, Error<'source>> {
        let mut expr = self.parse_mul()?;
        loop {
            if self.consume_reserved("+", "operator '+'")?.is_some() {
                let rhs = self.parse_mul()?;
                expr = Node::BinExpr {
                    op: BinOp::Add,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                };
            } else if self.consume_reserved("-", "operator '-'")?.is_some() {
                let rhs = self.parse_mul()?;
                expr = Node::BinExpr {
                    op: BinOp::Sub,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_relational(&mut self) -> Result<Node, Error<'source>> {
        let mut add = self.parse_add()?;
        loop {
            let rhs;
            let op;
            if self.consume_reserved(">", "operator '>'")?.is_some() {
                rhs = add;
                add = self.parse_add()?;
                op = BinOp::Lt;
            } else if self.consume_reserved(">=", "operator '>='")?.is_some() {
                rhs = add;
                add = self.parse_add()?;
                op = BinOp::Le;
            } else if self.consume_reserved("<", "operator '<'")?.is_some() {
                rhs = self.parse_add()?;
                op = BinOp::Lt;
            } else if self.consume_reserved("<=", "operator '<='")?.is_some() {
                rhs = self.parse_add()?;
                op = BinOp::Le;
            } else {
                break;
            }
            add = Node::BinExpr {
                op: op,
                lhs: Box::new(add),
                rhs: Box::new(rhs),
            }
        }
        Ok(add)
    }

    fn parse_equality(&mut self) -> Result<Node, Error<'source>> {
        let mut rel = self.parse_relational()?;
        loop {
            let rhs;
            let op;
            if self.consume_reserved("==", "operator '=='")?.is_some() {
                rhs = self.parse_relational()?;
                op = BinOp::Eq;
            } else if self.consume_reserved("!=", "operator '!='")?.is_some() {
                rhs = self.parse_relational()?;
                op = BinOp::Ne;
            } else {
                break;
            }
            rel = Node::BinExpr {
                op: op,
                lhs: Box::new(rel),
                rhs: Box::new(rhs),
            }
        }
        Ok(rel)
    }

    fn parse_expr(&mut self) -> Result<Node, Error<'source>> {
        self.parse_equality()
    }

    fn expect_eof(&mut self) -> Result<(), Error<'source>> {
        match self.tokens.next() {
            None => Ok(()),
            Some(Ok((token, loc))) => self.unexpected_token("EOF", token, loc),
            Some(Err(err)) => Err(err.into()),
        }
    }

    pub fn parse(&mut self) -> Result<Node, Error<'source>> {
        let expr = self.parse_expr()?;
        self.expect_eof()?;
        Ok(expr)
    }
}
