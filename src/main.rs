use std::env;

mod lexer;
use lexer::{Lexer, Location, Token};

pub fn describe_location(
    f: &mut std::fmt::Formatter,
    source: &str,
    location: Location,
) -> std::fmt::Result {
    let start = match location {
        Location::At(s) => s,
        Location::Span(s, _) => s,
    };
    if source.len() <= start {
        write!(f, " caused at end of input")
    } else {
        let line_number = source[..start].lines().count();
        let current_line_start = source[..start].rfind('\n').unwrap_or(0);
        let current_line_end = source[start..]
            .find(|c| c == '\r' || c == '\n')
            .map(|i| i + start)
            .unwrap_or(source.len());
        let current_line = &source[current_line_start..current_line_end];
        let column = start - current_line_start + 1;
        writeln!(f, " caused at {}:{}:", line_number, column)?;
        writeln!(f, "  | {}", current_line)?;
        match location {
            Location::At(_) => writeln!(f, "  . {:>width$}", "^", width = column),
            Location::Span(_, end) => writeln!(
                f,
                "  . {:>width$}{:~>trailing$}",
                "^",
                "",
                width = column,
                trailing = std::cmp::min(current_line_end, end) - column
            ),
        }
    }
}

enum ErrorKind<'a> {
    LexError(lexer::LexError<'a>),
    UnexpectedToken {
        got: Token<'a>,
        expected: Option<&'static str>,
    },
    UnexpectedEndOfFile {
        expected: Option<&'static str>,
    },
}

struct Error<'a> {
    kind: ErrorKind<'a>,
    location: Location,
    source: &'a str,
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

impl<'a> std::convert::From<lexer::LexError<'a>> for Error<'a> {
    fn from(v: lexer::LexError<'a>) -> Self {
        Self {
            location: v.location(),
            source: v.source(),
            kind: ErrorKind::LexError(v),
        }
    }
}

fn expect_number<'a>(lexer: &mut Lexer<'a>) -> Result<i32, Error<'a>> {
    match lexer.lex()? {
        Some((Token::Num(v), _)) => Ok(v),
        Some((t, loc)) => Err(Error {
            kind: ErrorKind::UnexpectedToken {
                got: t,
                expected: Some("number"),
            },
            location: loc,
            source: lexer.source(),
        }),
        None => Err(Error {
            kind: ErrorKind::UnexpectedEndOfFile {
                expected: Some("number"),
            },
            location: Location::At(lexer.source().len()),
            source: lexer.source(),
        }),
    }
}

fn gen(source: &str) -> Result<(), Error> {
    let mut lexer = Lexer::new(source);

    println!(".intel_syntax noprefix");
    println!(".global _main");
    println!("_main:");

    let v = expect_number(&mut lexer)?;
    println!("  mov rax, {}", v);

    loop {
        let token = lexer.lex().map_err(|err| Error::from(err))?;
        let (token, location) = match token {
            None => break,
            Some((token, location)) => (token, location),
        };
        match token {
            Token::Reserved("+") => {
                let rhs = expect_number(&mut lexer)?;
                println!("  add rax, {}", rhs);
            }
            Token::Reserved("-") => {
                let rhs = expect_number(&mut lexer)?;
                println!("  sub rax, {}", rhs);
            }
            t => {
                return Err(Error {
                    kind: ErrorKind::UnexpectedToken {
                        got: t,
                        expected: None,
                    },
                    location: location,
                    source: lexer.source(),
                })
            }
        }
    }

    println!("  ret");
    Ok(())
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args[0]);
        std::process::exit(1);
    }
    match gen(&args[1]) {
        Ok(()) => {}
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
