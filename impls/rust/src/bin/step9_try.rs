use mal::env::{Env, EnvRef};
use mal::reader;
use mal::types::{EvalError, MalType};
use mal::{core::NAMESPACE, types::UserFn};
use mal::{printer::pr_str, types::InternalFn};

use std::env;
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
        MalType::List(list, _) => {
            let evaluated: Result<Vec<_>, _> =
                list.into_iter().map(|e| eval(e, Rc::clone(&env))).collect();

            Ok(MalType::new_list(evaluated?))
        }
        MalType::Vector(vec, _) => {
            let evaluated: Result<Vec<_>, _> =
                vec.into_iter().map(|e| eval(e, Rc::clone(&env))).collect();

            Ok(MalType::new_vec(evaluated?))
        }
        MalType::Map(map, _) => {
            let evaluated: Result<HashMap<_, _>, _> = map
                .into_iter()
                .map(|(key, val)| Ok((key, eval(val, Rc::clone(&env))?)))
                .collect();

            Ok(MalType::new_map(evaluated?))
        }
        _ => Ok(ast),
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

fn apply_let(
    args: &[MalType],
    env: EnvRef,
) -> Result<(MalType, EnvRef), EvalError> {
    let local_env = Rc::new(Env::new(Some(env)));

    match &args {
        &[MalType::List(bindings, _), _] if bindings.len() % 2 != 0 => {
            Err(EvalError::InvalidLetBinding)
        }
        &[MalType::List(bindings, _), body]
        | &[MalType::Vector(bindings, _), body] => {
            for binding in bindings.chunks_exact(2) {
                if let (MalType::Symbol(sym), expr) = (&binding[0], &binding[1])
                {
                    let value = eval(expr.clone(), Rc::clone(&local_env))?;
                    local_env.set(sym, value);
                } else {
                    return Err(EvalError::InvalidLetBinding);
                }
            }
            Ok((body.clone(), local_env))
        }
        _ => Err(EvalError::ArityMismatch("let*", 2)),
    }
}

fn apply_do(
    mut exprs: Vec<MalType>,
    env: EnvRef,
) -> Result<MalType, EvalError> {
    if exprs.len() == 1 {
        Err(EvalError::EmptyDo)
    } else {
        let last = exprs.remove(exprs.len() - 1);
        for expr in exprs.into_iter().skip(1) {
            eval(expr, Rc::clone(&env))?;
        }

        Ok(last)
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
            (true, _) => Ok(then_branch.clone()),
            (false, [else_branch]) => Ok(else_branch.clone()),
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
        MalType::List(p, _) | MalType::Vector(p, _) => p,
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
    let tco_body = body.clone();
    let tco_parameters = parameters.clone();
    let tco_env = Rc::clone(&env);

    let lambda = move |args: &[MalType]| {
        let local_env = Env::from_bindings(
            Some(Rc::clone(&env)),
            parameters.clone(),
            args.to_vec(),
        )?;

        eval(body.clone(), Rc::new(local_env))
    };

    let fun = InternalFn(Rc::new(lambda));
    Ok(MalType::new_user_fn(Rc::new(UserFn::new(
        tco_body,
        tco_parameters,
        tco_env,
        fun,
    ))))
}

fn apply_quote(mut args: Vec<MalType>) -> Result<MalType, EvalError> {
    if args.len() == 2 {
        Ok(args.remove(1))
    } else {
        Err(EvalError::ArityMismatch("quote", 1))
    }
}

fn replace_splice_unquote(list: Vec<MalType>) -> Result<MalType, EvalError> {
    let splice_unquote = vec![MalType::Symbol("splice-unquote".to_string())];
    let concat = MalType::Symbol("concat".to_string());
    let cons = MalType::Symbol("cons".to_string());
    let mut processed_list = vec![];

    for elt in list.into_iter().rev() {
        match elt {
            MalType::List(list, _) if list.starts_with(&splice_unquote) => {
                if list.len() == 2 {
                    processed_list = vec![
                        concat.clone(),
                        list[1].clone(),
                        MalType::new_list(processed_list),
                    ];
                } else {
                    return Err(EvalError::ArityMismatch("splice-unquote", 1));
                }
            }
            _ => {
                processed_list = vec![
                    cons.clone(),
                    apply_quasiquote(elt)?,
                    MalType::new_list(processed_list),
                ]
            }
        }
    }

    Ok(MalType::new_list(processed_list))
}

fn apply_quasiquote(ast: MalType) -> Result<MalType, EvalError> {
    let unquote = MalType::Symbol("unquote".to_string());

    match ast {
        MalType::List(list, _) if list.starts_with(&[unquote]) => {
            if list.len() == 2 {
                Ok(list[1].clone())
            } else {
                Err(EvalError::ArityMismatch("unquote", 1))
            }
        }
        MalType::List(list, _) => replace_splice_unquote(list),
        MalType::Vector(vec, _) => match replace_splice_unquote(vec)? {
            MalType::List(spliced, _) => Ok(MalType::new_list(vec![
                MalType::Symbol("vec".to_string()),
                MalType::new_list(spliced),
            ])),
            _ => unreachable!(),
        },
        MalType::Map(_, _) | MalType::Symbol(_) => Ok(MalType::new_list(vec![
            MalType::Symbol("quote".to_string()),
            ast,
        ])),
        _ => Ok(ast),
    }
}

fn apply_defmacro(args: &[MalType], env: EnvRef) -> Result<MalType, EvalError> {
    match &args {
        [MalType::Symbol(name), expr] => {
            let value = match eval(expr.clone(), Rc::clone(&env))? {
                MalType::FnUser(func, _) => {
                    func.mark_as_macro();
                    MalType::new_user_fn(func)
                }
                value => value,
            };
            env.set(name, value.clone());
            Ok(value)
        }
        &[actual, _] => Err(EvalError::ExpectedSymbol(format!("{}", actual))),
        _ => Err(EvalError::ArityMismatch("def!", 2)),
    }
}

fn is_macro_call(
    ast: &MalType,
    env: EnvRef,
) -> Option<(Rc<UserFn>, &[MalType])> {
    let list = match ast {
        MalType::List(list, _) => list,
        _ => return None,
    };

    let (sym, args) = match list.as_slice() {
        [MalType::Symbol(sym), args @ ..] => (sym, args),
        _ => return None,
    };

    match env.get(sym) {
        Ok(MalType::FnUser(fun, _)) if fun.is_macro() => Some((fun, args)),
        _ => None,
    }
}

fn macroexpand(mut ast: MalType, env: EnvRef) -> Result<MalType, EvalError> {
    while let Some((fun, args)) = is_macro_call(&ast, Rc::clone(&env)) {
        ast = fun.fun.0(args)?;
    }

    Ok(ast)
}

fn apply_trycatch(
    args: &[MalType],
    env: Rc<Env>,
) -> Result<MalType, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityMismatchRange("try*", 1, 2));
    }

    let try_expr = args[0].clone();
    let exc = match eval(try_expr, Rc::clone(&env)) {
        Ok(good) => return Ok(good),
        Err(EvalError::MalException(value)) => value,
        Err(err) => MalType::String(format!("{}", err)),
    };

    if let Some(MalType::List(catch, _)) = args.get(1) {
        match catch.as_slice() {
            [MalType::Symbol(sym), MalType::Symbol(binding), catch_body]
                if sym == "catch*" =>
            {
                let catch_env = Env::from_bindings(
                    Some(env),
                    vec![binding.clone()],
                    vec![exc],
                )?;

                eval(catch_body.clone(), Rc::new(catch_env))
            }
            _ => Err(EvalError::InvalidCatchBinding),
        }
    } else {
        Err(EvalError::MalException(exc))
    }
}

fn eval(mut ast: MalType, mut env: EnvRef) -> Result<MalType, EvalError> {
    loop {
        ast = macroexpand(ast, Rc::clone(&env))?;
        match ast {
            MalType::List(ref list, _) if list.is_empty() => return Ok(ast),
            MalType::List(list, _) => match &list[0] {
                MalType::Symbol(sym) if sym == "def!" => {
                    return apply_def(&list[1..], env);
                }
                MalType::Symbol(sym) if sym == "defmacro!" => {
                    return apply_defmacro(&list[1..], env);
                }
                MalType::Symbol(sym) if sym == "let*" => {
                    let (let_ast, let_env) = apply_let(&list[1..], env)?;
                    ast = let_ast;
                    env = let_env;
                }
                MalType::Symbol(sym) if sym == "do" => {
                    ast = apply_do(list, Rc::clone(&env))?;
                }
                MalType::Symbol(sym) if sym == "if" => {
                    ast = apply_if(&list[1..], Rc::clone(&env))?;
                }
                MalType::Symbol(sym) if sym == "fn*" => {
                    return apply_lambda(list, env);
                }
                MalType::Symbol(sym) if sym == "quote" => {
                    return apply_quote(list);
                }
                MalType::Symbol(sym) if sym == "quasiquoteexpand" => {
                    return if list.len() == 2 {
                        apply_quasiquote(list[1].clone())
                    } else {
                        Err(EvalError::ArityMismatch("quasiquoteexpand", 1))
                    }
                }
                MalType::Symbol(sym) if sym == "quasiquote" => {
                    if list.len() == 2 {
                        ast = apply_quasiquote(list[1].clone())?;
                    } else {
                        return Err(EvalError::ArityMismatch("quasiquote", 1));
                    }
                }
                MalType::Symbol(sym) if sym == "macroexpand" => {
                    if list.len() == 2 {
                        return macroexpand(list[1].clone(), Rc::clone(&env));
                    } else {
                        return Err(EvalError::ArityMismatch("macroexpand", 1));
                    }
                }
                MalType::Symbol(sym) if sym == "try*" => {
                    return apply_trycatch(&list[1..], Rc::clone(&env))
                }
                _ => {
                    let call = match eval_ast(MalType::new_list(list), env)? {
                        MalType::List(c, _) => c,
                        _ => unreachable!(),
                    };

                    match call.as_slice() {
                        [MalType::Fn(f, _), args @ ..] => {
                            return f.0(args);
                        }
                        [MalType::FnUser(f, _), args @ ..] => {
                            ast = f.ast.clone();
                            env = Rc::new(Env::from_bindings(
                                Some(Rc::clone(&f.env)),
                                f.params.clone(),
                                args.to_vec(),
                            )?)
                        }
                        [value, ..] => {
                            return Err(EvalError::ExpectedFunction(format!(
                                "{}",
                                value
                            )));
                        }
                        _ => unreachable!(),
                    }
                }
            },
            _ => return eval_ast(ast, env),
        }
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
    let exit_code;

    for (name, func) in NAMESPACE {
        env.set(name, MalType::new_fn(InternalFn(Rc::new(func))));
    }

    let eval_env = Rc::clone(&env);
    let eval_func = move |args: &[MalType]| {
        if let [arg] = args {
            eval(arg.clone(), Rc::clone(&eval_env))
        } else {
            Err(EvalError::ArityMismatch("eval", 1))
        }
    };

    env.set("eval", MalType::new_fn(InternalFn(Rc::new(eval_func))));

    rep("(def! not (fn* (a) (if a false true)))", Rc::clone(&env));
    rep(
        r#"
(def! load-file
  (fn* (f)
    (eval (read-string (str "(do " (slurp f) "\nnil)")))))
"#,
        Rc::clone(&env),
    );

    rep(
        r#"
(defmacro! cond
  (fn* (& xs)
       (if (> (count xs) 0)
           (list 'if (first xs)
                 (if (> (count xs) 1)
                     (nth xs 1)
                   (throw "odd number of forms to cond"))
                 (cons 'cond (rest (rest xs)))))))
"#,
        Rc::clone(&env),
    );

    let cli_args: Vec<_> = env::args().collect();

    match cli_args.as_slice() {
        [_prog_name, mal_file, argv @ ..] => {
            env.set(
                "*ARGV*",
                MalType::new_list(
                    argv.iter()
                        .map(|s| MalType::String(s.to_string()))
                        .collect(),
                ),
            );
            // use eval instead of rep to avoid injection
            let result = eval(
                MalType::new_list(vec![
                    MalType::Symbol("load-file".to_string()),
                    MalType::String(mal_file.to_string()),
                ]),
                Rc::clone(&env),
            );

            match result {
                Err(e) => {
                    eprintln!("Couldn't load file '{}': {}", mal_file, e);
                    exit_code = 1;
                }
                _ => exit_code = 0,
            }
        }
        _ => {
            env.set("*ARGV*", MalType::new_list(vec![]));

            prompt();
            for line in stdin.lock().lines() {
                println!("{}", rep(&line.unwrap(), Rc::clone(&env)));
                prompt();
            }

            exit_code = 0;
        }
    }

    // prevent leaks because of reference cycle
    env.clear();

    std::process::exit(exit_code);
}
