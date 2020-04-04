use std::env;

mod compile;
mod lexer;
mod location;
mod parser;

use parser::Parser;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args[0]);
        std::process::exit(1);
    }

    let program = match Parser::new(&args[1]).parse_program() {
        Ok(program) => program,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    };

    match compile::gen_program(program) {
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
        Ok(()) => {},
    };
}
