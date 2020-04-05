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

struct CompileContext {
    last_label_id: usize,
}

impl CompileContext {
    fn new() -> Self {
        CompileContext { last_label_id: 0 }
    }

    fn next_label(&mut self, key: &'static str) -> String {
        let id = self.last_label_id + 1;
        self.last_label_id = id;
        format!(".L{}{}", key, id)
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

fn gen(node: Node, ctx: &mut CompileContext) -> Result<(), Error> {
    match node {
        Node::Num(v) => {
            println!("  push {}", v);
            return Ok(());
        }
        Node::BinExpr { op, lhs, rhs } => {
            gen(*lhs, ctx)?;
            gen(*rhs, ctx)?;
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
            gen(*rhs, ctx)?;
            println!("  pop rdi");
            println!("  pop rax");
            println!("  mov [rax], rdi");
            println!("  push rdi");
        }
        Node::Ident(_) => {
            gen_lval(node)?;
            println!("  pop rax");
            println!("  mov rax, [rax]");
            println!("  push rax");
        }
        Node::Return(value) => {
            gen(*value, ctx)?;
            println!("  pop rax");
            println!("  mov rsp, rbp");
            println!("  pop rbp");
            println!("  ret");
        }
        Node::IfElse {
            condition,
            then_expr,
            else_expr: None,
        } => {
            gen(*condition, ctx)?;
            println!("  pop rax");
            println!("  cmp rax, 0");
            let end_label = ctx.next_label("endif");
            println!("  je {}", end_label);
            gen(*then_expr, ctx)?;
            println!("{}:", end_label);
        }
        Node::IfElse {
            condition,
            then_expr,
            else_expr: Some(else_expr),
        } => {
            gen(*condition, ctx)?;
            println!("  pop rax");
            println!("  cmp rax, 0");
            let else_label = ctx.next_label("else");
            let endif_label = ctx.next_label("endif");
            println!("  je {}", else_label);
            // then
            gen(*then_expr, ctx)?;
            println!("  jmp {}", endif_label);
            // else
            println!("{}:", else_label);
            gen(*else_expr, ctx)?;
            // endif
            println!("{}:", endif_label);
        }
        Node::While {
            condition,
            statement,
        } => {
            let start_label = ctx.next_label("while");
            let end_label = ctx.next_label("endwhile");
            println!("{}:", start_label);
            gen(*condition, ctx)?;
            println!("  pop rax");
            println!("  cmp rax, 0");
            println!("  je {}", end_label);
            gen(*statement, ctx)?;
            println!("  jmp {}", start_label);
            println!("{}:", end_label);
        }
        Node::For {
            init_expr,
            condition_expr,
            update_expr,
            body,
        } => {
            let begin = ctx.next_label("for");
            let endfor = ctx.next_label("endfor");
            if let Some(init_expr) = init_expr {
                gen(*init_expr, ctx)?;
                println!("  pop rax");
            }
            println!("{}:", begin);
            if let Some(condition_expr) = condition_expr {
                gen(*condition_expr, ctx)?;
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je {}", endfor);
            }
            gen(*body, ctx)?;
            println!("  pop rax");
            if let Some(update_expr) = update_expr {
                gen(*update_expr, ctx)?;
                println!(" pop rax");
            }
            println!("  jmp {}", begin);
            println!("{}:", endfor);
        }
        Node::CompoundStatements(stmts) => {
            for stmt in stmts {
                gen(stmt, ctx)?;
                println!("  pop rax");
            }
        }
    }
    Ok(())
}

pub fn gen_program(stmts: Vec<Node>) -> Result<(), Error> {
    let mut ctx = CompileContext::new();

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    // Allocate memory for 26 variables.
    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, 208");

    for node in stmts {
        gen(node, &mut ctx)?;
        println!("  pop rax");
    }

    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
    Ok(())
}
