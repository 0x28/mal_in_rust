use std::{cmp::Ordering, fs, path::PathBuf};

use crate::reader;
use crate::{printer, types::MalAtom};

use super::types::{EvalError, MalType};

fn extract_numbers(args: &[MalType]) -> Option<Vec<i64>> {
    args.iter()
        .map(|value| match value {
            MalType::Integer(int) => Some(*int),
            _ => None,
        })
        .collect()
}

pub fn plus(args: &[MalType]) -> Result<MalType, EvalError> {
    let numbers = extract_numbers(args).ok_or_else(|| {
        EvalError::TypeMismatch("+".to_string(), "number".to_string())
    })?;

    Ok(MalType::Integer(numbers.iter().sum()))
}

pub fn mul(args: &[MalType]) -> Result<MalType, EvalError> {
    let numbers = extract_numbers(args).ok_or_else(|| {
        EvalError::TypeMismatch("*".to_string(), "number".to_string())
    })?;

    Ok(MalType::Integer(numbers.iter().product()))
}

pub fn minus(args: &[MalType]) -> Result<MalType, EvalError> {
    let numbers = extract_numbers(args).ok_or_else(|| {
        EvalError::TypeMismatch("-".to_string(), "number".to_string())
    })?;

    match numbers.as_slice() {
        [first] => Ok(MalType::Integer(-first)),
        [first, ref rest @ ..] => {
            Ok(MalType::Integer(rest.iter().fold(*first, |acc, v| acc - v)))
        }
        _ => Err(EvalError::ArityMismatch("-".to_string(), 1)),
    }
}

pub fn div(args: &[MalType]) -> Result<MalType, EvalError> {
    let numbers = extract_numbers(args).ok_or_else(|| {
        EvalError::TypeMismatch("/".to_string(), "number".to_string())
    })?;

    match numbers.as_slice() {
        [first] => Ok(MalType::Integer(1 / first)),
        [first, ref rest @ ..] => {
            match rest.iter().fold(Some(*first), |acc, v| {
                acc.map(|a| a.checked_div(*v)).flatten()
            }) {
                Some(quotient) => Ok(MalType::Integer(quotient)),
                None => Err(EvalError::DivisionByZero),
            }
        }
        _ => Err(EvalError::ArityMismatch("/".to_string(), 1)),
    }
}

#[allow(clippy::clippy::unnecessary_wraps)]
fn prn(args: &[MalType]) -> Result<MalType, EvalError> {
    let string = args
        .iter()
        .map(|arg| printer::pr_str(arg, true))
        .collect::<Vec<_>>()
        .join(" ");

    println!("{}", string);

    Ok(MalType::Nil)
}

#[allow(clippy::clippy::unnecessary_wraps)]
fn list(args: &[MalType]) -> Result<MalType, EvalError> {
    Ok(MalType::List(args.to_vec()))
}

fn is_list(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [value] = args {
        Ok(MalType::Boolean(matches!(value, MalType::List(_))))
    } else {
        Err(EvalError::ArityMismatch("list?".to_string(), 1))
    }
}

fn is_empty(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [value] = args {
        match value {
            MalType::List(list) => Ok(MalType::Boolean(list.is_empty())),
            MalType::Vector(vec) => Ok(MalType::Boolean(vec.is_empty())),
            _ => Err(EvalError::TypeMismatch(
                "empty?".to_string(),
                "list | vector".to_string(),
            )),
        }
    } else {
        Err(EvalError::ArityMismatch("empty?".to_string(), 1))
    }
}

fn count(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [value] = args {
        match value {
            MalType::List(list) => Ok(MalType::Integer(list.len() as i64)),
            MalType::Vector(vec) => Ok(MalType::Integer(vec.len() as i64)),
            MalType::Nil => Ok(MalType::Integer(0)),
            _ => Err(EvalError::TypeMismatch(
                "count".to_string(),
                "list | vector".to_string(),
            )),
        }
    } else {
        Err(EvalError::ArityMismatch("count".to_string(), 1))
    }
}

fn equal(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [left, right] = args {
        Ok(MalType::Boolean(left == right))
    } else {
        Err(EvalError::ArityMismatch("=".to_string(), 2))
    }
}

fn cmp(
    name: String,
    ordering: &[Ordering],
    args: &[MalType],
) -> Result<MalType, EvalError> {
    match args {
        [MalType::Integer(left), MalType::Integer(right)] => {
            Ok(MalType::Boolean(ordering.contains(&left.cmp(right))))
        }
        [_, _] => Err(EvalError::TypeMismatch(name, "number".to_string())),
        _ => Err(EvalError::ArityMismatch(name, 2)),
    }
}

fn less(args: &[MalType]) -> Result<MalType, EvalError> {
    cmp("<".to_string(), &[Ordering::Less], args)
}

fn greater(args: &[MalType]) -> Result<MalType, EvalError> {
    cmp(">".to_string(), &[Ordering::Greater], args)
}

fn greater_equal(args: &[MalType]) -> Result<MalType, EvalError> {
    cmp(
        ">=".to_string(),
        &[Ordering::Greater, Ordering::Equal],
        args,
    )
}

fn less_equal(args: &[MalType]) -> Result<MalType, EvalError> {
    cmp("<=".to_string(), &[Ordering::Less, Ordering::Equal], args)
}

#[allow(clippy::clippy::unnecessary_wraps)]
fn pr_str(args: &[MalType]) -> Result<MalType, EvalError> {
    let string = args
        .iter()
        .map(|arg| printer::pr_str(arg, true))
        .collect::<Vec<_>>()
        .join(" ");

    Ok(MalType::String(string))
}

#[allow(clippy::clippy::unnecessary_wraps)]
fn str(args: &[MalType]) -> Result<MalType, EvalError> {
    let string = args
        .iter()
        .map(|arg| printer::pr_str(arg, false))
        .collect::<Vec<_>>()
        .concat();

    Ok(MalType::String(string))
}

#[allow(clippy::clippy::unnecessary_wraps)]
fn println(args: &[MalType]) -> Result<MalType, EvalError> {
    let string = args
        .iter()
        .map(|arg| printer::pr_str(arg, false))
        .collect::<Vec<_>>()
        .join(" ");

    println!("{}", string);

    Ok(MalType::Nil)
}

fn read_string(args: &[MalType]) -> Result<MalType, EvalError> {
    let fn_name = String::from("read-string");
    match args {
        [MalType::String(string)] => {
            reader::read_str(string).map_err(EvalError::ReaderError)
        }
        [_] => Err(EvalError::TypeMismatch(fn_name, "string".to_string())),
        _ => Err(EvalError::ArityMismatch(fn_name, 1)),
    }
}

fn slurp(args: &[MalType]) -> Result<MalType, EvalError> {
    let fn_name = String::from("slurp");
    match args {
        [MalType::String(string)] => fs::read_to_string(PathBuf::from(string))
            .map(MalType::String)
            .map_err(EvalError::InOutError),
        [_] => Err(EvalError::TypeMismatch(fn_name, "string".to_string())),
        _ => Err(EvalError::ArityMismatch(fn_name, 1)),
    }
}

fn atom(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [arg] = args {
        Ok(MalType::Atom(MalAtom::new(arg.clone())))
    } else {
        Err(EvalError::ArityMismatch("atom".to_string(), 1))
    }
}

fn is_atom(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Atom(_)] => Ok(MalType::Boolean(true)),
        [_] => Ok(MalType::Boolean(false)),
        _ => Err(EvalError::ArityMismatch("atom?".to_string(), 1)),
    }
}

fn deref(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Atom(atom)] => Ok(atom.deref()),
        [_] => Err(EvalError::TypeMismatch(
            "deref".to_string(),
            "atom".to_string(),
        )),
        _ => Err(EvalError::ArityMismatch("deref".to_string(), 1)),
    }
}

fn reset(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Atom(atom), value] => {
            atom.reset(value.clone());
            Ok(value.clone())
        }
        [_] => Err(EvalError::TypeMismatch(
            "reset!".to_string(),
            "atom".to_string(),
        )),
        _ => Err(EvalError::ArityMismatch("reset!".to_string(), 2)),
    }
}

fn swap(args: &[MalType]) -> Result<MalType, EvalError> {
    let swap_name = String::from("swap!");

    match args {
        [MalType::Atom(atom), MalType::Fn(fun), rest @ ..] => {
            let mut new_args = vec![atom.deref()];
            new_args.append(&mut rest.to_vec());

            let result = fun.0(&new_args)?;
            atom.reset(result.clone());
            Ok(result)
        }
        [MalType::Atom(atom), MalType::FnTco(tco_fun), rest @ ..] => {
            let mut new_args = vec![atom.deref()];
            new_args.append(&mut rest.to_vec());

            let result = tco_fun.fun.0(&new_args)?;
            atom.reset(result.clone());
            Ok(result)
        }
        [_, _, ..] => Err(EvalError::TypeMismatchMultiple(
            swap_name,
            vec!["atom".to_string(), "function".to_string()],
        )),
        _ => Err(EvalError::ArityMismatchRange(swap_name, 2, usize::MAX)),
    }
}

type Namespace =
    &'static [(&'static str, fn(&[MalType]) -> Result<MalType, EvalError>)];

pub const NAMESPACE: Namespace = &[
    ("+", plus),
    ("-", minus),
    ("*", mul),
    ("/", div),
    ("prn", prn),
    ("list", list),
    ("list?", is_list),
    ("empty?", is_empty),
    ("count", count),
    ("=", equal),
    ("<", less),
    (">", greater),
    (">=", greater_equal),
    ("<=", less_equal),
    ("pr-str", pr_str),
    ("str", str),
    ("println", println),
    ("read-string", read_string),
    ("slurp", slurp),
    ("atom", atom),
    ("atom?", is_atom),
    ("deref", deref),
    ("reset!", reset),
    ("swap!", swap),
];
