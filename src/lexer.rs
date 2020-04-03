use crate::location::Location;

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone)]
enum LexErrorKind {
    UnexpectedCharacter(char),
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
        }
    }
}

#[derive(PartialEq)]
pub enum Token<'a> {
    Reserved(&'a str),
    Num(i32),
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Reserved(s) => write!(f, "keyword {:?}", s),
            Token::Num(i) => write!(f, "{}", i),
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

    fn eat_if(&mut self, predicate: impl Fn(char) -> bool) -> bool {
        match self.char_indices.peek() {
            Some((_, c)) if predicate(*c) => {
                self.char_indices.next();
                true
            }
            _ => false,
        }
    }

    fn eat_whitespace(&mut self) -> bool {
        self.eat_if(|c| match c {
            ' ' | '\t' | '\n' | '\r' => true,
            _ => false,
        })
    }

    fn lex_number(&mut self) -> Lexed<'a> {
        let start = self.offset();
        while self.eat_if(|c| c.is_digit(10)) {}
        let end = self.offset();

        if start == end {
            return None;
        }

        let v = self.source[start..end]
            .parse()
            .expect("expected valid i32 value");
        Some((Token::Num(v), Location::Span(start, end)))
    }

    pub fn lex(&mut self) -> LexResult<'a> {
        while self.eat_whitespace() {}

        let offset = self.offset();

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
