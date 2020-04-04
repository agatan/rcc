use std::env;

mod lexer;
mod location;
mod parser;
mod compile;

use parser::Parser;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args[0]);
        std::process::exit(1);
    }
    let node = match Parser::new(&args[1]).parse_program() {
        Ok(mut node) => node.pop().unwrap(),
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    };
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    compile::gen(node);

    println!("  pop rax");
    println!("  ret");
}
