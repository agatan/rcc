use std::env;

mod lexer;
mod location;
mod parser;

use parser::{BinOp, Node, Parser};

fn gen(node: Node) {
    match node {
        Node::Num(v) => {
            println!("  push {}", v);
            return;
        }
        Node::BinExpr { op, lhs, rhs } => {
            gen(*lhs);
            gen(*rhs);
            println!("  pop rdi");
            println!("  pop rax");
            match op {
                BinOp::Add => println!("  add rax, rdi"),
                BinOp::Sub => println!("  sub rax, rdi"),
                BinOp::Mul => println!("  imul rax, rdi"),
                BinOp::Div => {
                    println!("  cqo");
                    println!("  idiv rdi");
                }
            }
            println!("  push rax");
        }
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args[0]);
        std::process::exit(1);
    }
    let node = match Parser::new(&args[1]).parse_expr() {
        Ok(node) => node,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    };
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    gen(node);

    println!("  pop rax");
    println!("  ret");
}
