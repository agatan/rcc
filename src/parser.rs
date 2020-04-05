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

#[derive(Clone)]
pub struct LVar<'source> {
    name: &'source str,
    offset: usize,
}

impl<'source> LVar<'source> {
    pub fn offset(&self) -> usize {
        self.offset
    }
}

pub enum Node<'source> {
    BinExpr {
        op: BinOp,
        lhs: Box<Node<'source>>,
        rhs: Box<Node<'source>>,
    },
    Num(i32),
    Ident(LVar<'source>),
    Assign {
        lhs: Box<Node<'source>>,
        rhs: Box<Node<'source>>,
    },
    Return(Box<Node<'source>>),
    IfElse {
        condition: Box<Node<'source>>,
        then_expr: Box<Node<'source>>,
        else_expr: Option<Box<Node<'source>>>,
    },
    While {
        condition: Box<Node<'source>>,
        statement: Box<Node<'source>>,
    },
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

pub struct ParserContext<'source> {
    locals: Vec<LVar<'source>>,
}

impl<'source> ParserContext<'source> {
    fn new() -> Self {
        Self { locals: Vec::new() }
    }

    fn find_or_declare(&mut self, name: &'source str) -> LVar<'source> {
        if let Some(lvar) = self.locals.iter().find(|l| l.name == name) {
            return lvar.clone();
        }
        let lvar = LVar {
            name,
            offset: (self.locals.len() + 1) * 8,
        };
        self.locals.push(lvar.clone());
        lvar
    }
}

pub struct Parser<'source> {
    source: &'source str,
    tokens: Tokens<'source>,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Self {
        Parser {
            source,
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

    fn consume_operator(
        &mut self,
        keyword: &'static str,
        expected: &'static str,
    ) -> Result<Option<(Token, Location)>, Error<'source>> {
        match self.peek()? {
            Some((Token::Operator(s), _)) if s == &keyword => {
                let (token, loc) = self.next_token(expected)?;
                Ok(Some((token, loc)))
            }
            _ => Ok(None),
        }
    }

    fn expect_operator(
        &mut self,
        keyword: &'static str,
        expected: &'static str,
    ) -> Result<(Token, Location), Error<'source>> {
        match self.next_token(expected)? {
            (Token::Operator(s), loc) if s == keyword => Ok((Token::Operator(s), loc)),
            (t, loc) => self.unexpected_token(expected, t, loc),
        }
    }

    fn parse_primary(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        let (token, loc) = self.next_token("primary expression")?;
        match token {
            Token::Operator("(") => {
                let expr = self.parse_expr(ctx)?;
                self.expect_operator(")", "closing ')'")?;
                Ok(expr)
            }
            Token::Num(v) => Ok(Node::Num(v)),
            Token::Ident(s) => Ok(Node::Ident(ctx.find_or_declare(s))),
            _ => self.unexpected_token("primary expression", token, loc),
        }
    }

    fn parse_unary(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        if self.consume_operator("+", "unary '+'")?.is_some() {
            return self.parse_primary(ctx);
        }
        if self.consume_operator("-", "unary '-'")?.is_some() {
            return Ok(Node::BinExpr {
                op: BinOp::Sub,
                lhs: Box::new(Node::Num(0)),
                rhs: self.parse_primary(ctx).map(Box::new)?,
            });
        }
        self.parse_primary(ctx)
    }

    fn parse_mul(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        let mut mul = self.parse_unary(ctx)?;
        loop {
            if self.consume_operator("*", "operator '*'")?.is_some() {
                let rhs = self.parse_unary(ctx)?;
                mul = Node::BinExpr {
                    op: BinOp::Mul,
                    lhs: Box::new(mul),
                    rhs: Box::new(rhs),
                };
            } else if self.consume_operator("/", "operator '/'")?.is_some() {
                let rhs = self.parse_unary(ctx)?;
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

    fn parse_add(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        let mut expr = self.parse_mul(ctx)?;
        loop {
            if self.consume_operator("+", "operator '+'")?.is_some() {
                let rhs = self.parse_mul(ctx)?;
                expr = Node::BinExpr {
                    op: BinOp::Add,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                };
            } else if self.consume_operator("-", "operator '-'")?.is_some() {
                let rhs = self.parse_mul(ctx)?;
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

    fn parse_relational(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        let mut add = self.parse_add(ctx)?;
        loop {
            let rhs;
            let op;
            if self.consume_operator(">", "operator '>'")?.is_some() {
                rhs = add;
                add = self.parse_add(ctx)?;
                op = BinOp::Lt;
            } else if self.consume_operator(">=", "operator '>='")?.is_some() {
                rhs = add;
                add = self.parse_add(ctx)?;
                op = BinOp::Le;
            } else if self.consume_operator("<", "operator '<'")?.is_some() {
                rhs = self.parse_add(ctx)?;
                op = BinOp::Lt;
            } else if self.consume_operator("<=", "operator '<='")?.is_some() {
                rhs = self.parse_add(ctx)?;
                op = BinOp::Le;
            } else {
                break;
            }
            add = Node::BinExpr {
                op,
                lhs: Box::new(add),
                rhs: Box::new(rhs),
            }
        }
        Ok(add)
    }

    fn parse_equality(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        let mut rel = self.parse_relational(ctx)?;
        loop {
            let rhs;
            let op;
            if self.consume_operator("==", "operator '=='")?.is_some() {
                rhs = self.parse_relational(ctx)?;
                op = BinOp::Eq;
            } else if self.consume_operator("!=", "operator '!='")?.is_some() {
                rhs = self.parse_relational(ctx)?;
                op = BinOp::Ne;
            } else {
                break;
            }
            rel = Node::BinExpr {
                op,
                lhs: Box::new(rel),
                rhs: Box::new(rhs),
            }
        }
        Ok(rel)
    }

    fn parse_assign(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        let equality = self.parse_equality(ctx)?;
        if self
            .consume_operator("=", "assignment operator '='")?
            .is_some()
        {
            let value = self.parse_assign(ctx)?;
            return Ok(Node::Assign {
                lhs: Box::new(equality),
                rhs: Box::new(value),
            });
        }
        Ok(equality)
    }

    fn parse_expr(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        self.parse_assign(ctx)
    }

    fn parse_statement(
        &mut self,
        ctx: &mut ParserContext<'source>,
    ) -> Result<Node<'source>, Error<'source>> {
        match self.peek()? {
            Some((Token::Return, _)) => {
                self.next_token("keyword 'return'")?;
                let value = self.parse_expr(ctx)?;
                self.expect_operator(";", "';'")?;
                Ok(Node::Return(Box::new(value)))
            }
            Some((Token::If, _)) => {
                self.next_token("keyword 'if'")?;
                self.expect_operator("(", "opening '('")?;
                let condition = self.parse_expr(ctx)?;
                self.expect_operator(")", "opening ')'")?;
                let then_else = self.parse_statement(ctx)?;
                if let Some((Token::Else, _)) = self.peek()? {
                    self.next_token("keyword 'else'")?;
                    let else_expr = self.parse_statement(ctx)?;
                    Ok(Node::IfElse {
                        condition: Box::new(condition),
                        then_expr: Box::new(then_else),
                        else_expr: Some(Box::new(else_expr)),
                    })
                } else {
                    Ok(Node::IfElse {
                        condition: Box::new(condition),
                        then_expr: Box::new(then_else),
                        else_expr: None,
                    })
                }
            }
            Some((Token::While, _)) => {
                self.next_token("keyword 'while'")?;
                self.expect_operator("(", "openinig '('")?;
                let condition = self.parse_expr(ctx)?;
                self.expect_operator(")", "closing ')'")?;
                let stmt = self.parse_statement(ctx)?;
                Ok(Node::While {
                    condition: Box::new(condition),
                    statement: Box::new(stmt),
                })
            }
            _ => {
                let expr = self.parse_expr(ctx)?;
                self.expect_operator(";", "';'")?;
                Ok(expr)
            }
        }
    }

    fn at_eof(&self) -> bool {
        self.tokens.peek().is_none()
    }

    pub fn parse_program(&mut self) -> Result<Vec<Node<'source>>, Error<'source>> {
        let mut context = ParserContext::new();
        let mut stmts = Vec::new();
        while !self.at_eof() {
            stmts.push(self.parse_statement(&mut context)?);
        }
        Ok(stmts)
    }
}
