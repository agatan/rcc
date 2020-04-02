macro_rules! error {
    ($($arg:tt)*) => {
        {
            eprintln!($($arg)*);
            std::process::exit(1)
        }
    };
}

pub enum Token<'a> {
    Reserved(&'a str),
    Num(i32),
    Eof,
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Reserved(s) => write!(f, "{}", s),
            Token::Num(i) => write!(f, "{}", i),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

pub struct Tokenizer<'a> {
    char_indices: std::iter::Peekable<std::str::CharIndices<'a>>,
    source: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Tokenizer {
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

    fn lex_number(&mut self) -> Option<Token<'a>> {
        let start = self.offset();
        while self.eat_if(|c| c.is_digit(10)) {}
        let end = self.offset();

        let v = self.source[start..end]
            .parse()
            .expect("expected valid i32 value");
        Some(Token::Num(v))
    }

    pub fn tokenize(&mut self) -> Token<'a> {
        while self.eat_whitespace() {}

        let (offset, ch) = match self.char_indices.peek() {
            None => return Token::Eof,
            Some(x) => *x,
        };

        if self.eat_char('+') || self.eat_char('-') {
            return Token::Reserved(&self.source[offset..offset + 1]);
        }

        if let Some(token) = self.lex_number() {
            return token;
        }

        error!("unepxcted charactor: {}", ch);
    }
}
