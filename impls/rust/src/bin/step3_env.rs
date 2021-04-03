use mal::env::Env;
use mal::core::{div, minus, mul, plus};
use mal::reader;
use mal::types::{EvalError, MalType};
use mal::{printer::pr_str, types::MalFunc};
use reader::ReaderError;

use std::io::{self, BufRead, Write};
use std::{collections::HashMap, rc::Rc};

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

fn eval_ast(ast: MalType, env: &mut Env) -> Result<MalType, EvalError> {
    match ast {
        MalType::Symbol(symbol) => {
            let value = env.get(&symbol)?;
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

fn apply_def(args: &[MalType], env: &mut Env) -> Result<MalType, EvalError> {
    match &args {
        [MalType::Symbol(name), expr] => {
            let value = eval(expr.clone(), env)?;
            env.set(name, value.clone());
            Ok(value)
        }
        &[actual, _] => Err(EvalError::ExpectedSymbol(format!("{}", actual))),
        _ => Err(EvalError::ArityMismatch("def!".to_string(), 2)),
    }
}

fn apply_let(args: &[MalType], env: &Env) -> Result<MalType, EvalError> {
    let mut local_env = Env::new(Some(env));

    match &args {
        &[MalType::List(bindings), _] if bindings.len() % 2 != 0 => {
            Err(EvalError::InvalidLetBinding)
        }
        &[MalType::List(bindings), body]
        | &[MalType::Vector(bindings), body] => {
            for binding in bindings.chunks_exact(2) {
                if let (MalType::Symbol(sym), expr) = (&binding[0], &binding[1])
                {
                    let value = eval(expr.clone(), &mut local_env)?;
                    local_env.set(sym, value);
                } else {
                    return Err(EvalError::InvalidLetBinding);
                }
            }
            eval(body.clone(), &mut local_env)
        }
        _ => Err(EvalError::ArityMismatch("let*".to_string(), 2)),
    }
}

fn eval(ast: MalType, env: &mut Env) -> Result<MalType, EvalError> {
    match ast {
        MalType::List(ref list) if list.is_empty() => Ok(ast),
        MalType::List(list) => match &list[0] {
            MalType::Symbol(sym) if sym == "def!" => apply_def(&list[1..], env),
            MalType::Symbol(sym) if sym == "let*" => apply_let(&list[1..], env),
            _ => {
                if let MalType::List(call) = eval_ast(MalType::List(list), env)?
                {
                    apply(&call[0], &call[1..])
                } else {
                    panic!("Eval of a list didn't result in a list!")
                }
            }
        },
        _ => eval_ast(ast, env),
    }
}

fn print(ast: &MalType) -> String {
    pr_str(ast, true)
}

fn rep(line: &str, env: &mut Env) -> String {
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
    let mut env = Env::new(None);

    env.set("+", MalType::Fn(MalFunc(Rc::new(plus))));
    env.set("*", MalType::Fn(MalFunc(Rc::new(mul))));
    env.set("-", MalType::Fn(MalFunc(Rc::new(minus))));
    env.set("/", MalType::Fn(MalFunc(Rc::new(div))));

    prompt();
    for line in stdin.lock().lines() {
        println!("{}", rep(&line.unwrap(), &mut env));
        prompt();
    }
}
