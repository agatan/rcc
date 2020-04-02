use std::env;

macro_rules! error {
    ($($arg:tt)*) => {
        {
            eprintln!($($arg)*);
            std::process::exit(1)
        }
    };
}

struct Tokenizer<'a> {
    input: &'a str,
    p: &'a str,
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a str) -> Self {
        Tokenizer { input: input, p: input }
    }

    fn succ(&mut self, n: usize) {
        self.p = &self.p[n..];
    }

    fn consume(&mut self, s: &'static str) -> bool {
        if self.p.starts_with(s) {
            self.succ(s.len());
            return true;
        }
        false
    }

    fn strtol(&mut self) -> i32 {
        let leading_digits = self.p.chars().take_while(|c| c.is_digit(10)).count();
        let digits = &self.p[..leading_digits];
        self.succ(leading_digits);
        digits.parse().expect("expected a number")
    }

    fn is_empty(&self) -> bool {
        self.p.is_empty()
    }

    fn display_attention(&self) -> DisplayAttention<'_> {
        DisplayAttention(self)
    }
}

struct DisplayAttention<'a>(&'a Tokenizer<'a>);

impl <'a> std::fmt::Display for DisplayAttention<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.p)
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

    let v = tokenizer.strtol();
    println!("  mov rax, {}", v);

    while !tokenizer.is_empty() {
        if tokenizer.consume("+") {
            println!("  add rax, {}", tokenizer.strtol());
            continue;
        }
        if tokenizer.consume("-") {
            println!("  sub rax, {}", tokenizer.strtol());
            continue;
        }
        error!("unexpected charactor:\n{}", tokenizer.display_attention());
    }

    println!("  ret");
}
