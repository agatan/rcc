use crate::parser::{BinOp, Node};

pub enum ErrorKind {
    AssignmentToRValue,
}

pub struct Error {
    kind: ErrorKind,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            ErrorKind::AssignmentToRValue => f.write_str("expression is not assignable"),
        }
    }
}

fn gen_lval(node: Node) -> Result<(), Error> {
    let lvar = match node {
        Node::Ident(lvar) => lvar,
        _ => {
            return Err(Error {
                kind: ErrorKind::AssignmentToRValue,
            });
        }
    };
    println!("  mov rax, rbp");
    println!("  sub rax, {}", lvar.offset());
    println!("  push rax");
    Ok(())
}

fn gen(node: Node) -> Result<(), Error> {
    match node {
        Node::Num(v) => {
            println!("  push {}", v);
            return Ok(());
        }
        Node::BinExpr { op, lhs, rhs } => {
            gen(*lhs)?;
            gen(*rhs)?;
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
                BinOp::Eq => {
                    println!("  cmp rax, rdi");
                    println!("  sete al");
                    println!("  movzb rax, al");
                }
                BinOp::Ne => {
                    println!("  cmp rax, rdi");
                    println!("  setne al");
                    println!("  movzb rax, al");
                }
                BinOp::Lt => {
                    println!("  cmp rax, rdi");
                    println!("  setl al");
                    println!("  movzb rax, al");
                }
                BinOp::Le => {
                    println!("  cmp rax, rdi");
                    println!("  setle al");
                    println!("  movzb rax, al");
                }
            }
            println!("  push rax");
        }
        Node::Assign { lhs, rhs } => {
            gen_lval(*lhs)?;
            gen(*rhs)?;
            println!("  pop rdi");
            println!("  pop rax");
            println!("  mov [rax], rdi");
            println!("  push rdi");
        },
        Node::Ident(_) => {
            gen_lval(node)?;
            println!("  pop rax");
            println!("  mov rax, [rax]");
            println!("  push rax");
        },
    }
    Ok(())
}

pub fn gen_program(stmts: Vec<Node>) -> Result<(), Error> {
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    // Allocate memory for 26 variables.
    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, 208");

    for node in stmts {
        gen(node)?;
        println!("  pop rax");
    }

    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
    Ok(())
}
