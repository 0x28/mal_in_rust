use mal::core::{div, minus, mul, plus};
use mal::reader;
use mal::types::{EvalError, MalType};
use mal::{printer::pr_str, types::InternalFn};
use std::io::{self, BufRead, Write};
use std::{collections::HashMap, rc::Rc};

type Env = HashMap<String, MalType>;

fn read(input: &str) -> Option<MalType> {
    match reader::read_str(input) {
        Ok(ast) => Some(ast),
        Err(e) => {
            eprintln!("Error: {}", e);
            None
        }
    }
}

fn eval_ast(ast: MalType, env: &Env) -> Result<MalType, EvalError> {
    match ast {
        MalType::Symbol(symbol) => {
            let value = env.get(&symbol).ok_or_else(|| {
                EvalError::UnknownVariable(symbol.to_string())
            })?;
            Ok(value.clone())
        }
        MalType::List(list) => {
            let evaluated: Result<Vec<_>, _> =
                list.into_iter().map(|e| eval(e, env)).collect();

            Ok(MalType::List(evaluated?))
        }
        MalType::Vector(vec) => {
            let evaluated: Result<Vec<_>, _> =
                vec.into_iter().map(|e| eval(e, env)).collect();

            Ok(MalType::Vector(evaluated?))
        }
        MalType::Map(map) => {
            let evaluated: Result<HashMap<_, _>, _> = map
                .into_iter()
                .map(|(key, val)| Ok((key, eval(val, env)?)))
                .collect();

            Ok(MalType::Map(evaluated?))
        }
        _ => Ok(ast),
    }
}

fn apply(func: &MalType, args: &[MalType]) -> Result<MalType, EvalError> {
    match func {
        MalType::Fn(f) => f.0(args),
        _ => Err(EvalError::ExpectedFunction(format!("{}", func))),
    }
}

fn eval(ast: MalType, env: &Env) -> Result<MalType, EvalError> {
    match ast {
        MalType::List(ref list) if list.is_empty() => Ok(ast),
        list @ MalType::List(_) => {
            if let MalType::List(call) = eval_ast(list, env)? {
                apply(&call[0], &call[1..])
            } else {
                panic!("Eval of a list didn't result in a list!")
            }
        }
        _ => eval_ast(ast, env),
    }
}

fn print(ast: &MalType) -> String {
    pr_str(ast, true)
}

fn rep(line: &str, env: &Env) -> String {
    match read(line) {
        Some(ast) => match eval(ast, env) {
            Ok(value) => print(&value),
            Err(e) => {
                eprintln!("Error: {}", e);
                String::new()
            }
        },
        None => String::new(),
    }
}

fn prompt() {
    print!("user> ");
    io::stdout().flush().unwrap();
}

fn main() {
    let stdin = io::stdin();
    let mut env = HashMap::new();

    env.insert("+".to_string(), MalType::Fn(InternalFn(Rc::new(plus))));
    env.insert("*".to_string(), MalType::Fn(InternalFn(Rc::new(mul))));
    env.insert("-".to_string(), MalType::Fn(InternalFn(Rc::new(minus))));
    env.insert("/".to_string(), MalType::Fn(InternalFn(Rc::new(div))));

    prompt();
    for line in stdin.lock().lines() {
        println!("{}", rep(&line.unwrap(), &env));
        prompt();
    }
}
