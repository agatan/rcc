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

pub struct CompileContext {
    last_label_id: usize,
    rsp_alignment: isize,
}

impl CompileContext {
    pub fn new() -> Self {
        CompileContext {
            last_label_id: 0,
            rsp_alignment: 0,
        }
    }

    fn emit_pop(&mut self, reg: &str) {
        self.rsp_alignment -= 8;
        println!("  pop {}", reg);
    }

    fn emit_push_value(&mut self, value: i32) {
        self.rsp_alignment += 8;
        println!("  push {}", value);
    }

    fn emit_push_reg(&mut self, reg: &str) {
        self.rsp_alignment += 8;
        println!("  push {}", reg);
    }

    fn next_label(&mut self, key: &'static str) -> String {
        let id = self.last_label_id + 1;
        self.last_label_id = id;
        format!(".L{}{}", key, id)
    }

    fn gen_lval(&mut self, node: Node) -> Result<(), Error> {
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
        self.emit_push_reg("rax");
        Ok(())
    }

    fn gen(&mut self, node: Node) -> Result<(), Error> {
        match node {
            Node::Num(v) => {
                self.emit_push_value(v);
                return Ok(());
            }
            Node::BinExpr { op, lhs, rhs } => {
                self.gen(*lhs)?;
                self.gen(*rhs)?;
                self.emit_pop("rdi");
                self.emit_pop("rax");
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
                self.emit_push_reg("rax");
            }
            Node::Assign { lhs, rhs } => {
                self.gen_lval(*lhs)?;
                self.gen(*rhs)?;
                self.emit_pop("rdi");
                self.emit_pop("rax");
                println!("  mov [rax], rdi");
                self.emit_push_reg("rdi");
            }
            Node::Ident(_) => {
                self.gen_lval(node)?;
                self.emit_pop("rax");
                println!("  mov rax, [rax]");
                self.emit_push_reg("rax");
            }
            Node::Call { function_name, args} => {
                assert!(args.len() <= 6, "rcc doesn't support function calls with more than 6 arguments yet.");
                let registered = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for (arg, reg) in args.into_iter().zip(&registered) {
                    self.gen(arg)?;
                    self.emit_pop(reg);
                }
                println!("  call {}", function_name);
                self.emit_push_reg("rax");
            }
            Node::Return(value) => {
                self.gen(*value)?;
                self.emit_pop("rax");
                println!("  mov rsp, rbp");
                self.emit_pop("rbp");
                println!("  ret");
            }
            Node::IfElse {
                condition,
                then_expr,
                else_expr: None,
            } => {
                self.gen(*condition)?;
                self.emit_pop("rax");
                println!("  cmp rax, 0");
                let end_label = self.next_label("endif");
                println!("  je {}", end_label);
                self.gen(*then_expr)?;
                println!("{}:", end_label);
            }
            Node::IfElse {
                condition,
                then_expr,
                else_expr: Some(else_expr),
            } => {
                self.gen(*condition)?;
                self.emit_pop("rax");
                println!("  cmp rax, 0");
                let else_label = self.next_label("else");
                let endif_label = self.next_label("endif");
                println!("  je {}", else_label);
                // then
                self.gen(*then_expr)?;
                println!("  jmp {}", endif_label);
                // else
                println!("{}:", else_label);
                self.gen(*else_expr)?;
                // endif
                println!("{}:", endif_label);
            }
            Node::While {
                condition,
                statement,
            } => {
                let start_label = self.next_label("while");
                let end_label = self.next_label("endwhile");
                println!("{}:", start_label);
                self.gen(*condition)?;
                self.emit_pop("rax");
                println!("  cmp rax, 0");
                println!("  je {}", end_label);
                self.gen(*statement)?;
                println!("  jmp {}", start_label);
                println!("{}:", end_label);
            }
            Node::For {
                init_expr,
                condition_expr,
                update_expr,
                body,
            } => {
                let begin = self.next_label("for");
                let endfor = self.next_label("endfor");
                if let Some(init_expr) = init_expr {
                    self.gen(*init_expr)?;
                    self.emit_pop("rax");
                }
                println!("{}:", begin);
                if let Some(condition_expr) = condition_expr {
                    self.gen(*condition_expr)?;
                    self.emit_pop("rax");
                    println!("  cmp rax, 0");
                    println!("  je {}", endfor);
                }
                self.gen(*body)?;
                self.emit_pop("rax");
                if let Some(update_expr) = update_expr {
                    self.gen(*update_expr)?;
                    println!(" pop rax");
                }
                println!("  jmp {}", begin);
                println!("{}:", endfor);
            }
            Node::CompoundStatements(stmts) => {
                for stmt in stmts {
                    self.gen(stmt)?;
                    self.emit_pop("rax");
                }
            }
        }
        Ok(())
    }

    pub fn gen_program(&mut self, stmts: Vec<Node>) -> Result<(), Error> {
        println!(".intel_syntax noprefix");
        println!(".global main");
        println!("main:");

        // Allocate memory for 26 variables.
        println!("  push rbp");
        println!("  mov rbp, rsp");
        println!("  sub rsp, 208");

        for node in stmts {
            self.gen(node)?;
            self.emit_pop("rax");
        }

        println!("  mov rsp, rbp");
        self.emit_pop("rbp");
        println!("  ret");
        Ok(())
    }
}


pub fn gen_program(nodes: Vec<Node>) -> Result<(), Error> {
    CompileContext::new().gen_program(nodes)
}
