use std::env;

mod lexer;
use lexer::{Tokenizer, Token};

macro_rules! error {
    ($($arg:tt)*) => {
        {
            eprintln!($($arg)*);
            std::process::exit(1)
        }
    };
}

fn expect_number<'a>(tokenizer: &mut Tokenizer<'a>) -> i32 {
    match tokenizer.tokenize() {
        Token::Num(v) => v,
        t => error!("unexpected token: {}", t),
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        error!("{}: invalid number of arguments", args[0])
    }
    let mut tokenizer = Tokenizer::new(&args[1]);

    println!(".intel_syntax noprefix");
    println!(".global _main");
    println!("_main:");

    let v = match tokenizer.tokenize() {
        Token::Num(v) => v,
        t => error!("unexpected token: {}", t),
    };
    println!("  mov rax, {}", v);

    loop {
        let token = tokenizer.tokenize();
        match token {
            Token::Eof => break,
            Token::Reserved("+") => {
                println!("  add rax, {}", expect_number(&mut tokenizer));
            }
            Token::Reserved("-") => {
                println!("  sub rax, {}", expect_number(&mut tokenizer));
            }
            t => error!("unexpected token: {}", t),
        }
    }

    println!("  ret");
}
