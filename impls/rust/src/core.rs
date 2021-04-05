use std::cmp::Ordering;

use crate::printer;

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
];
