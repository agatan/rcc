use std::env;

macro_rules! error {
    ($($arg:tt)*) => {
        {
            eprintln!($($arg)*);
            std::process::exit(1)
        }
    };
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        error!("{}: invalid number of arguments", args[0])
    }
    let v = args[1]
        .parse::<i32>()
        .unwrap_or_else(|e| error!("{}: expect a number", e));

    println!(".intel_syntax noprefix");
    println!(".global _main");
    println!("_main:");

    println!("  mov rax, {}", v);

    println!("  ret");
}
