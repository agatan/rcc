use crate::lexer::{LexError, Lexer, Token};
use crate::location::{describe_location, Location};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum BinOp {
    Add,
    Sub,
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

    fn peek(
        &mut self,
        expected: Option<&'static str>,
    ) -> Result<(&Token, Location), Error<'source>> {
        match self.tokens.peek() {
            None => self.unexpected_eof(expected),
            Some(Err(ref err)) => Err(err.clone().into()),
            Some(Ok((token, loc))) => Ok((token, *loc)),
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

    fn expect_number(&mut self) -> Result<i32, Error<'source>> {
        let (token, loc) = self.next_token("number")?;
        match token {
            Token::Num(v) => Ok(v),
            _ => self.error(
                ErrorKind::UnexpectedToken {
                    got: token,
                    expected: Some("number"),
                },
                loc,
            ),
        }
    }

    fn parse_primary(&mut self) -> Result<Node, Error<'source>> {
        self.expect_number().map(|v| Node::Num(v))
    }

    fn parse_mul(&mut self) -> Result<Node, Error<'source>> {
        let lhs = self.parse_primary()?;
        Ok(lhs)
    }

    pub fn parse_expr(&mut self) -> Result<Node, Error<'source>> {
        self.parse_mul()
    }
}
