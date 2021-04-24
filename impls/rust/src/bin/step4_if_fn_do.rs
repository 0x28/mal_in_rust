use mal::core::NAMESPACE;
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

fn apply_do(exprs: Vec<MalType>, env: EnvRef) -> Result<MalType, EvalError> {
    let values: Result<Vec<_>, _> = exprs
        .into_iter()
        .skip(1)
        .map(|v| eval(v, Rc::clone(&env)))
        .collect();

    match values?.into_iter().last() {
        Some(value) => Ok(value),
        None => Err(EvalError::EmptyDo),
    }
}

fn apply_if(exprs: &[MalType], env: EnvRef) -> Result<MalType, EvalError> {
    if let [pred, then_branch, rest @ ..] = exprs {
        let cond = match eval(pred.clone(), Rc::clone(&env))? {
            MalType::Nil => false,
            MalType::Boolean(b) => b,
            _ => true,
        };

        match (cond, rest) {
            (true, _) => eval(then_branch.clone(), env),
            (false, [else_branch]) => eval(else_branch.clone(), env),
            (false, []) => Ok(MalType::Nil),
            _ => Err(EvalError::ArityMismatchRange("if", 2, 3)),
        }
    } else {
        Err(EvalError::ArityMismatchRange("if", 2, 3))
    }
}

fn apply_lambda(
    mut exprs: Vec<MalType>,
    env: EnvRef,
) -> Result<MalType, EvalError> {
    if exprs.len() != 3 {
        return Err(EvalError::ArityMismatch("fn*", 2));
    }

    let body = exprs.remove(2);
    let parameters = exprs.remove(1);
    let _name = exprs.remove(0);

    let parameters = match parameters {
        MalType::List(p) | MalType::Vector(p) => p,
        _ => return Err(EvalError::InvalidFnBinding),
    };

    let parameters: Option<Vec<_>> = parameters
        .into_iter()
        .map(|p| match p {
            MalType::Symbol(sym) => Some(sym),
            _ => None,
        })
        .collect();

    let parameters = parameters.ok_or(EvalError::InvalidFnBinding)?;

    let lambda = move |args: &[MalType]| {
        let local_env = Env::from_bindings(
            Some(Rc::clone(&env)),
            parameters.clone(),
            args.to_vec(),
        )?;

        eval(body.clone(), Rc::new(local_env))
    };

    Ok(MalType::Fn(InternalFn(Rc::new(lambda))))
}

fn eval(ast: MalType, env: EnvRef) -> Result<MalType, EvalError> {
    match ast {
        MalType::List(ref list) if list.is_empty() => Ok(ast),
        MalType::List(list) => match &list[0] {
            MalType::Symbol(sym) if sym == "def!" => apply_def(&list[1..], env),
            MalType::Symbol(sym) if sym == "let*" => apply_let(&list[1..], env),
            MalType::Symbol(sym) if sym == "do" => apply_do(list, env),
            MalType::Symbol(sym) if sym == "if" => apply_if(&list[1..], env),
            MalType::Symbol(sym) if sym == "fn*" => apply_lambda(list, env),
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

    for (name, func) in NAMESPACE {
        env.set(name, MalType::Fn(InternalFn(Rc::new(func))));
    }

    rep("(def! not (fn* (a) (if a false true)))", Rc::clone(&env));
    prompt();
    for line in stdin.lock().lines() {
        println!("{}", rep(&line.unwrap(), Rc::clone(&env)));
        prompt();
    }
}
