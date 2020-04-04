use crate::parser::{BinOp, Node};

pub fn gen(node: Node) {
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
        Node::Assign { .. } => todo!(),
        Node::Ident(_) => todo!(),
    }
}
