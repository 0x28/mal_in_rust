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
    let numbers =
        extract_numbers(args).ok_or(EvalError::TypeMismatch("+", "number"))?;

    Ok(MalType::Integer(numbers.iter().sum()))
}

pub fn mul(args: &[MalType]) -> Result<MalType, EvalError> {
    let numbers =
        extract_numbers(args).ok_or(EvalError::TypeMismatch("*", "number"))?;

    Ok(MalType::Integer(numbers.iter().product()))
}

pub fn minus(args: &[MalType]) -> Result<MalType, EvalError> {
    let numbers =
        extract_numbers(args).ok_or(EvalError::TypeMismatch("-", "number"))?;

    match numbers.as_slice() {
        [first] => Ok(MalType::Integer(-first)),
        [first, ref rest @ ..] => {
            Ok(MalType::Integer(rest.iter().fold(*first, |acc, v| acc - v)))
        }
        _ => Err(EvalError::ArityMismatch("-", 1)),
    }
}

pub fn div(args: &[MalType]) -> Result<MalType, EvalError> {
    let numbers =
        extract_numbers(args).ok_or(EvalError::TypeMismatch("/", "number"))?;

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
        _ => Err(EvalError::ArityMismatch("/", 1)),
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
        Err(EvalError::ArityMismatch("list?", 1))
    }
}

fn is_empty(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [value] = args {
        match value {
            MalType::List(list) => Ok(MalType::Boolean(list.is_empty())),
            MalType::Vector(vec) => Ok(MalType::Boolean(vec.is_empty())),
            _ => Err(EvalError::TypeMismatch("empty?", "list | vector")),
        }
    } else {
        Err(EvalError::ArityMismatch("empty?", 1))
    }
}

fn count(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [value] = args {
        match value {
            MalType::List(list) => Ok(MalType::Integer(list.len() as i64)),
            MalType::Vector(vec) => Ok(MalType::Integer(vec.len() as i64)),
            MalType::Nil => Ok(MalType::Integer(0)),
            _ => Err(EvalError::TypeMismatch("count", "list | vector")),
        }
    } else {
        Err(EvalError::ArityMismatch("count", 1))
    }
}

fn equal(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [left, right] = args {
        Ok(MalType::Boolean(left == right))
    } else {
        Err(EvalError::ArityMismatch("=", 2))
    }
}

fn cmp(
    name: &'static str,
    ordering: &[Ordering],
    args: &[MalType],
) -> Result<MalType, EvalError> {
    match args {
        [MalType::Integer(left), MalType::Integer(right)] => {
            Ok(MalType::Boolean(ordering.contains(&left.cmp(right))))
        }
        [_, _] => Err(EvalError::TypeMismatch(name, "number")),
        _ => Err(EvalError::ArityMismatch(name, 2)),
    }
}

fn less(args: &[MalType]) -> Result<MalType, EvalError> {
    cmp("<", &[Ordering::Less], args)
}

fn greater(args: &[MalType]) -> Result<MalType, EvalError> {
    cmp(">", &[Ordering::Greater], args)
}

fn greater_equal(args: &[MalType]) -> Result<MalType, EvalError> {
    cmp(">=", &[Ordering::Greater, Ordering::Equal], args)
}

fn less_equal(args: &[MalType]) -> Result<MalType, EvalError> {
    cmp("<=", &[Ordering::Less, Ordering::Equal], args)
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
    let fn_name = "read-string";
    match args {
        [MalType::String(string)] => {
            reader::read_str(string).map_err(EvalError::ReaderError)
        }
        [_] => Err(EvalError::TypeMismatch(fn_name, "string")),
        _ => Err(EvalError::ArityMismatch(fn_name, 1)),
    }
}

fn slurp(args: &[MalType]) -> Result<MalType, EvalError> {
    let fn_name = "slurp";
    match args {
        [MalType::String(string)] => fs::read_to_string(PathBuf::from(string))
            .map(MalType::String)
            .map_err(EvalError::InOutError),
        [_] => Err(EvalError::TypeMismatch(fn_name, "string")),
        _ => Err(EvalError::ArityMismatch(fn_name, 1)),
    }
}

fn atom(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [arg] = args {
        Ok(MalType::Atom(MalAtom::new(arg.clone())))
    } else {
        Err(EvalError::ArityMismatch("atom", 1))
    }
}

fn is_atom(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Atom(_)] => Ok(MalType::Boolean(true)),
        [_] => Ok(MalType::Boolean(false)),
        _ => Err(EvalError::ArityMismatch("atom?", 1)),
    }
}

fn deref(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Atom(atom)] => Ok(atom.deref()),
        [_] => Err(EvalError::TypeMismatch("deref", "atom")),
        _ => Err(EvalError::ArityMismatch("deref", 1)),
    }
}

fn reset(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Atom(atom), value] => {
            atom.reset(value.clone());
            Ok(value.clone())
        }
        [_] => Err(EvalError::TypeMismatch("reset!", "atom")),
        _ => Err(EvalError::ArityMismatch("reset!", 2)),
    }
}

fn swap(args: &[MalType]) -> Result<MalType, EvalError> {
    let swap_name = "swap!";

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
            vec!["atom", "function"],
        )),
        _ => Err(EvalError::ArityMismatchRange(swap_name, 2, usize::MAX)),
    }
}

fn cons(args: &[MalType]) -> Result<MalType, EvalError> {
    let cons_name = "cons";

    match args {
        [value, MalType::List(list)] | [value, MalType::Vector(list)] => {
            let mut new_list = vec![value.clone()];
            new_list.extend_from_slice(list);

            Ok(MalType::List(new_list))
        }
        [_, _] => Err(EvalError::TypeMismatchMultiple(
            cons_name,
            vec!["any", "list"],
        )),
        _ => Err(EvalError::ArityMismatch(cons_name, 2)),
    }
}

fn concat(args: &[MalType]) -> Result<MalType, EvalError> {
    let mut list = vec![];

    for arg in args {
        match arg {
            MalType::List(other) | MalType::Vector(other) => {
                list.extend_from_slice(other)
            }
            _ => return Err(EvalError::TypeMismatch("concat", "list")),
        }
    }

    Ok(MalType::List(list))
}

fn vec(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::List(list)] => Ok(MalType::Vector(list.clone())),
        [MalType::Vector(vec)] => Ok(MalType::Vector(vec.clone())),
        [_] => Err(EvalError::TypeMismatch("vec", "list | vector")),
        _ => Err(EvalError::ArityMismatch("vec", 1)),
    }
}

fn nth(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::List(elements), MalType::Integer(idx)]
        | [MalType::Vector(elements), MalType::Integer(idx)] => {
            let idx = *idx as usize;
            Ok(elements
                .get(idx)
                .ok_or_else(|| EvalError::InvalidIndex(elements.len(), idx))?
                .clone())
        }
        [_, _] => Err(EvalError::TypeMismatchMultiple(
            "nth",
            vec!["list | vector", "integer"],
        )),
        _ => Err(EvalError::ArityMismatch("nth", 2)),
    }
}

fn first(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Nil] => Ok(MalType::Nil),
        [MalType::List(elements)] | [MalType::Vector(elements)] => {
            Ok(elements.iter().next().unwrap_or(&MalType::Nil).clone())
        }
        [_] => Err(EvalError::TypeMismatch("first", "list | vector")),
        _ => Err(EvalError::ArityMismatch("first", 1)),
    }
}

fn rest(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Nil] => Ok(MalType::List(vec![])),
        [MalType::List(elements)] | [MalType::Vector(elements)] => {
            Ok(MalType::List(elements.iter().skip(1).cloned().collect()))
        }
        [_] => Err(EvalError::TypeMismatch("rest", "list | vector")),
        _ => Err(EvalError::ArityMismatch("rest", 1)),
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
    ("cons", cons),
    ("concat", concat),
    ("vec", vec),
    ("nth", nth),
    ("first", first),
    ("rest", rest),
];
