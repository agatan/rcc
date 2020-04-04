use crate::location::Location;

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone)]
enum LexErrorKind {
    UnexpectedCharacter(char),
    UnexpectedEndOfFile,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone)]
pub struct LexError<'a> {
    kind: LexErrorKind,
    location: Location,
    source: &'a str,
}

impl<'a> LexError<'a> {
    pub fn location(&self) -> Location {
        self.location
    }

    pub fn source(&self) -> &'a str {
        self.source
    }
}

impl<'a> std::fmt::Display for LexError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use LexErrorKind::*;
        match self.kind {
            UnexpectedCharacter(c) => write!(f, "unexpected character: {:?}", c),
            UnexpectedEndOfFile => write!(f, "reached EOF"),
        }
    }
}

#[derive(PartialEq)]
pub enum Token<'a> {
    Reserved(&'a str),
    Num(i32),
    Ident(char),
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Reserved(s) => write!(f, "keyword {:?}", s),
            Token::Num(i) => write!(f, "{}", i),
            Token::Ident(ch) => write!(f, "'{}'", ch),
        }
    }
}

pub type Lexed<'a> = Option<(Token<'a>, Location)>;
pub type Result<'a, T> = std::result::Result<T, LexError<'a>>;
pub type LexResult<'a> = Result<'a, Lexed<'a>>;

pub struct Lexer<'a> {
    char_indices: std::iter::Peekable<std::str::CharIndices<'a>>,
    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            char_indices: input.char_indices().peekable(),
            source: input,
        }
    }

    fn offset(&mut self) -> usize {
        match self.char_indices.peek() {
            Some((offset, _)) => *offset,
            None => self.source.len(),
        }
    }

    fn eat_char(&mut self, expect: char) -> bool {
        match self.char_indices.peek() {
            Some((_, c)) if *c == expect => {
                self.char_indices.next();
                true
            }
            _ => false,
        }
    }

    fn unexpected_eof<T>(&self) -> Result<'a, T> {
        Err(LexError {
            kind: LexErrorKind::UnexpectedEndOfFile,
            source: self.source,
            location: Location::At(self.source.len()),
        })
    }

    fn unexpected_char<T>(&self, ch: char, location: Location) -> Result<'a, T> {
        Err(LexError {
            kind: LexErrorKind::UnexpectedCharacter(ch),
            source: self.source,
            location: location,
        })
    }

    fn expect_char(&mut self, expect: char) -> Result<'a, char> {
        match self.char_indices.peek().cloned() {
            Some((_, c)) if c == expect => {
                self.char_indices.next();
                Ok(c)
            }
            Some((i, c)) => self.unexpected_char(c, Location::At(i)),
            None => self.unexpected_eof(),
        }
    }

    fn eat_if(&mut self, predicate: impl Fn(char) -> bool) -> Option<char> {
        match self.char_indices.peek().cloned() {
            Some((_, c)) if predicate(c) => {
                self.char_indices.next();
                Some(c)
            }
            _ => None,
        }
    }

    fn eat_whitespace(&mut self) -> bool {
        self.eat_if(|c| match c {
            ' ' | '\t' | '\n' | '\r' => true,
            _ => false,
        })
        .is_some()
    }

    fn lex_number(&mut self) -> Lexed<'a> {
        let start = self.offset();
        while self.eat_if(|c| c.is_digit(10)).is_some() {}
        let end = self.offset();

        if start == end {
            return None;
        }

        let v = self.source[start..end]
            .parse()
            .expect("expected valid i32 value");
        Some((Token::Num(v), Location::Span(start, end)))
    }

    fn new_reserved(&self, start: usize, end: usize) -> (Token<'a>, Location) {
        (
            Token::Reserved(&self.source[start..end]),
            Location::Span(start, end),
        )
    }

    pub fn lex(&mut self) -> LexResult<'a> {
        while self.eat_whitespace() {}

        let offset = self.offset();

        if self.eat_char(';') {
            return Ok(Some(self.new_reserved(offset, offset + 1)));
        }

        if self.eat_char('+')
            || self.eat_char('-')
            || self.eat_char('*')
            || self.eat_char('/')
            || self.eat_char('(')
            || self.eat_char(')')
        {
            return Ok(Some((
                Token::Reserved(&self.source[offset..offset + 1]),
                Location::Span(offset, offset + 1),
            )));
        }

        if self.eat_char('=') {
            if self.eat_char('=') {
                return Ok(Some(self.new_reserved(offset, offset + 2)));
            }
            return Ok(Some(self.new_reserved(offset, offset + 1)));
        }
        if self.eat_char('!') {
            self.expect_char('=')?;
            return Ok(Some(self.new_reserved(offset, offset + 2)));
        }
        if self.eat_char('<') {
            if self.eat_char('=') {
                return Ok(Some(self.new_reserved(offset, offset + 2)));
            }
            return Ok(Some(self.new_reserved(offset, offset + 1)));
        }
        if self.eat_char('>') {
            if self.eat_char('=') {
                return Ok(Some(self.new_reserved(offset, offset + 2)));
            }
            return Ok(Some(self.new_reserved(offset, offset + 1)));
        }

        if let Some(c) = self.eat_if(|c| 'a' <= c && c <= 'z') {
            return Ok(Some((Token::Ident(c), Location::Span(offset, offset + 1))));
        }

        if let Some(token) = self.lex_number() {
            return Ok(Some(token));
        }

        match self.char_indices.peek() {
            None => Ok(None),
            Some(&(offset, ch)) => Err(LexError {
                kind: LexErrorKind::UnexpectedCharacter(ch),
                source: self.source,
                location: Location::At(offset),
            }),
        }
    }
}

impl<'a> std::iter::Iterator for Lexer<'a> {
    type Item = Result<'a, (Token<'a>, Location)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex().transpose()
    }
}
