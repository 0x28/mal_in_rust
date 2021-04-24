use mal::core::{div, minus, mul, plus};
use mal::env::{Env, EnvRef};
use mal::reader;
use mal::types::{EvalError, MalType};
use mal::{printer::pr_str, types::InternalFn};

use std::io::{self, BufRead, Write};
use std::{collections::HashMap, rc::Rc};

fn read(input: &str) -> Option<MalType> {
    match reader::read_str(input) {
        Ok(ast) => Some(ast),
        Err(e) => {
            eprintln!("Error: {}", e);
            None
        }
    }
}

fn eval_ast(ast: MalType, env: EnvRef) -> Result<MalType, EvalError> {
    match ast {
        MalType::Symbol(symbol) => Ok(env.get(&symbol)?),
        MalType::List(list) => {
            let evaluated: Result<Vec<_>, _> =
                list.into_iter().map(|e| eval(e, Rc::clone(&env))).collect();

            Ok(MalType::List(evaluated?))
        }
        MalType::Vector(vec) => {
            let evaluated: Result<Vec<_>, _> =
                vec.into_iter().map(|e| eval(e, Rc::clone(&env))).collect();

            Ok(MalType::Vector(evaluated?))
        }
        MalType::Map(map) => {
            let evaluated: Result<HashMap<_, _>, _> = map
                .into_iter()
                .map(|(key, val)| Ok((key, eval(val, Rc::clone(&env))?)))
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

fn apply_def(args: &[MalType], env: EnvRef) -> Result<MalType, EvalError> {
    match &args {
        [MalType::Symbol(name), expr] => {
            let value = eval(expr.clone(), Rc::clone(&env))?;
            env.set(name, value.clone());
            Ok(value)
        }
        &[actual, _] => Err(EvalError::ExpectedSymbol(format!("{}", actual))),
        _ => Err(EvalError::ArityMismatch("def!", 2)),
    }
}

fn apply_let(args: &[MalType], env: EnvRef) -> Result<MalType, EvalError> {
    let local_env = Rc::new(Env::new(Some(env)));

    match &args {
        &[MalType::List(bindings), _] if bindings.len() % 2 != 0 => {
            Err(EvalError::InvalidLetBinding)
        }
        &[MalType::List(bindings), body]
        | &[MalType::Vector(bindings), body] => {
            for binding in bindings.chunks_exact(2) {
                if let (MalType::Symbol(sym), expr) = (&binding[0], &binding[1])
                {
                    let value = eval(expr.clone(), Rc::clone(&local_env))?;
                    local_env.set(sym, value);
                } else {
                    return Err(EvalError::InvalidLetBinding);
                }
            }
            eval(body.clone(), local_env)
        }
        _ => Err(EvalError::ArityMismatch("let*", 2)),
    }
}

fn eval(ast: MalType, env: EnvRef) -> Result<MalType, EvalError> {
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

fn rep(line: &str, env: EnvRef) -> String {
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
    let env = Rc::new(Env::new(None));

    env.set("+", MalType::Fn(InternalFn(Rc::new(plus))));
    env.set("*", MalType::Fn(InternalFn(Rc::new(mul))));
    env.set("-", MalType::Fn(InternalFn(Rc::new(minus))));
    env.set("/", MalType::Fn(InternalFn(Rc::new(div))));

    prompt();
    for line in stdin.lock().lines() {
        println!("{}", rep(&line.unwrap(), Rc::clone(&env)));
        prompt();
    }
}
