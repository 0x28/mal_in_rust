use std::io::{self, BufRead, Write};

fn read(arg: &str) -> &str {
    arg
}

fn eval(arg: &str) -> &str {
    arg
}

fn print(arg: &str) -> &str {
    arg
}

fn rep(line: &str) -> &str {
    print(eval(read(line)))
}

fn prompt() {
    print!("user> ");
    io::stdout().flush().unwrap();
}

fn main() {
    let stdin = io::stdin();

    prompt();
    for line in stdin.lock().lines() {
        println!("{}", rep(&line.unwrap()));
        prompt();
    }
}
