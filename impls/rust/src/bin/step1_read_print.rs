use mal::types::MalType;
use mal::{printer::pr_str, reader};
use reader::ReaderError;
use std::io::{self, BufRead, Write};

fn read(input: &str) -> Option<MalType> {
    match reader::read_str(input) {
        Ok(ast) => Some(ast),
        Err(ReaderError::Comment) => None,
        Err(e) => {
            eprintln!("Error: {}", e);
            None
        }
    }
}

fn eval(ast: MalType) -> MalType {
    ast
}

fn print(ast: &MalType) -> String {
    pr_str(ast, true)
}

fn rep(line: &str) -> String {
    match read(line) {
        Some(ast) => print(&eval(ast)),
        None => String::new(),
    }
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
