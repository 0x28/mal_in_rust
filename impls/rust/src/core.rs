use std::fs;
use std::io;
use std::{cmp::Ordering, io::Write, path::PathBuf};
use std::collections::HashMap;

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
    Ok(MalType::new_list(args.to_vec()))
}

macro_rules! mal_predicate{
    ($($pattern: pat)|+ $(if $guard: expr)?, $fn: ident, $name: expr) => {
    fn $fn(args: &[MalType]) -> Result<MalType, EvalError> {
        match args {
            [value] => Ok(MalType::Boolean(
                matches!(value, $($pattern)|+ $(if $guard)?))),
            _ => Err(EvalError::ArityMismatch($name, 1)),
        }
    }
}}

mal_predicate!(MalType::List(_, _), is_list, "list?");

fn is_empty(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [value] = args {
        match value {
            MalType::List(list, _) => Ok(MalType::Boolean(list.is_empty())),
            MalType::Vector(vec, _) => Ok(MalType::Boolean(vec.is_empty())),
            _ => Err(EvalError::TypeMismatch("empty?", "list | vector")),
        }
    } else {
        Err(EvalError::ArityMismatch("empty?", 1))
    }
}

fn count(args: &[MalType]) -> Result<MalType, EvalError> {
    if let [value] = args {
        match value {
            MalType::List(list, _) => Ok(MalType::Integer(list.len() as i64)),
            MalType::Vector(vec, _) => Ok(MalType::Integer(vec.len() as i64)),
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

mal_predicate!(MalType::Atom(_), is_atom, "atom?");

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
        [MalType::Atom(atom), MalType::Fn(fun, _), rest @ ..] => {
            let mut new_args = vec![atom.deref()];
            new_args.append(&mut rest.to_vec());

            let result = fun.0(&new_args)?;
            atom.reset(result.clone());
            Ok(result)
        }
        [MalType::Atom(atom), MalType::FnUser(tco_fun, _), rest @ ..] => {
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
        [value, MalType::List(list, _)] | [value, MalType::Vector(list, _)] => {
            let mut new_list = vec![value.clone()];
            new_list.extend_from_slice(list);

            Ok(MalType::new_list(new_list))
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
            MalType::List(other, _) | MalType::Vector(other, _) => {
                list.extend_from_slice(other)
            }
            _ => return Err(EvalError::TypeMismatch("concat", "list")),
        }
    }

    Ok(MalType::new_list(list))
}

fn vec(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::List(list, _)] => Ok(MalType::new_vec(list.clone())),
        [MalType::Vector(vec, _)] => Ok(MalType::new_vec(vec.clone())),
        [_] => Err(EvalError::TypeMismatch("vec", "list | vector")),
        _ => Err(EvalError::ArityMismatch("vec", 1)),
    }
}

fn nth(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::List(elements, _), MalType::Integer(idx)]
        | [MalType::Vector(elements, _), MalType::Integer(idx)] => {
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
        [MalType::List(elements, _)] | [MalType::Vector(elements, _)] => {
            Ok(elements.iter().next().unwrap_or(&MalType::Nil).clone())
        }
        [_] => Err(EvalError::TypeMismatch("first", "list | vector")),
        _ => Err(EvalError::ArityMismatch("first", 1)),
    }
}

fn rest(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Nil] => Ok(MalType::new_list(vec![])),
        [MalType::List(elements, _)] | [MalType::Vector(elements, _)] => {
            Ok(MalType::new_list(elements.iter().skip(1).cloned().collect()))
        }
        [_] => Err(EvalError::TypeMismatch("rest", "list | vector")),
        _ => Err(EvalError::ArityMismatch("rest", 1)),
    }
}

fn throw(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [arg] => Err(EvalError::MalException(arg.clone())),
        _ => Err(EvalError::ArityMismatch("throw", 1)),
    }
}

fn apply(args: &[MalType]) -> Result<MalType, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::ArityMismatchRange("apply", 2, usize::MAX));
    }

    let types = vec!["function", "any...", "list | vector"];

    let mut new_args = vec![];
    new_args.extend_from_slice(&args[1..args.len() - 1]);

    match args.last() {
        Some(MalType::Vector(a, _)) | Some(MalType::List(a, _)) => {
            new_args.extend(a.clone());
        }
        _ => return Err(EvalError::TypeMismatchMultiple("apply", types)),
    }

    match &args[0] {
        MalType::Fn(f, _) => f.0(&new_args),
        MalType::FnUser(f, _) => f.fun.0(&new_args),
        _ => Err(EvalError::TypeMismatchMultiple("apply", types)),
    }
}

fn map(args: &[MalType]) -> Result<MalType, EvalError> {
    let mapped_list: Result<Vec<_>, EvalError> = match args {
        [MalType::Fn(f, _), MalType::Vector(xs, _)]
        | [MalType::Fn(f, _), MalType::List(xs, _)] => {
            xs.chunks(1).map(|value| f.0(value)).collect()
        }
        [MalType::FnUser(f, _), MalType::Vector(xs, _)]
        | [MalType::FnUser(f, _), MalType::List(xs, _)] => {
            xs.chunks(1).map(|value| f.fun.0(value)).collect()
        }
        [_, _] => {
            return Err(EvalError::TypeMismatchMultiple(
                "map",
                vec!["function", "list | vector"],
            ))
        }
        _ => return Err(EvalError::ArityMismatch("map", 2)),
    };

    Ok(MalType::new_list(mapped_list?))
}

fn symbol(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::String(s)] => Ok(MalType::Symbol(s.to_string())),
        [_] => Err(EvalError::TypeMismatch("symbol", "string")),
        _ => Err(EvalError::ArityMismatch("symbol", 1)),
    }
}

fn keyword(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::String(s)] => {
            if s.starts_with(MalType::KEYWORD_SYMBOL) {
                Ok(MalType::String(s.clone()))
            } else {
                Ok(MalType::String(format!("{}{}", MalType::KEYWORD_SYMBOL, s)))
            }
        }
        [_] => Err(EvalError::TypeMismatch("symbol", "string")),
        _ => Err(EvalError::ArityMismatch("symbol", 1)),
    }
}

fn vector(args: &[MalType]) -> Result<MalType, EvalError> {
    Ok(MalType::new_vec(args.to_vec()))
}

fn hash_map(args: &[MalType]) -> Result<MalType, EvalError> {
    let mut map = HashMap::new();

    if args.len() % 2 != 0 {
        return Err(EvalError::ArityMismatchEven("hash-map"));
    }

    for key_value in args.chunks_exact(2) {
        match key_value {
            [MalType::String(key), value] => {
                map.insert(key.clone(), value.clone());
            }
            [_, _] => {
                return Err(EvalError::TypeMismatch(
                    "hash-map",
                    "[string, any]...",
                ))
            }
            _ => unreachable!(),
        }
    }

    Ok(MalType::new_map(map))
}

fn assoc(args: &[MalType]) -> Result<MalType, EvalError> {
    if args.len() % 2 == 0 {
        return Err(EvalError::ArityMismatchOdd("assoc"));
    }

    let types = vec!["map", "[string, any]..."];

    match args {
        [MalType::Map(map, _), rest @ ..] => {
            let mut new_map = map.clone();
            for key_value in rest.chunks_exact(2) {
                match key_value {
                    [MalType::String(key), value] => {
                        new_map.insert(key.clone(), value.clone());
                    }
                    [_, _] => {
                        return Err(EvalError::TypeMismatchMultiple(
                            "assoc", types,
                        ))
                    }
                    _ => unreachable!(),
                }
            }
            Ok(MalType::new_map(new_map))
        }
        [_, ..] => Err(EvalError::TypeMismatchMultiple("assoc", types)),
        _ => Err(EvalError::ArityMismatchRange("assoc", 1, usize::MAX)),
    }
}

fn dissoc(args: &[MalType]) -> Result<MalType, EvalError> {
    let types = vec!["map", "string..."];

    match args {
        [MalType::Map(hashmap, _), keys @ ..] => {
            let mut new_map = hashmap.clone();

            for key in keys {
                match key {
                    MalType::String(key) => {
                        new_map.remove(key);
                    }
                    _ => {
                        return Err(EvalError::TypeMismatchMultiple(
                            "assoc", types,
                        ))
                    }
                }
            }

            Ok(MalType::new_map(new_map))
        }
        [_, ..] => Err(EvalError::TypeMismatchMultiple("assoc", types)),
        _ => Err(EvalError::ArityMismatchRange("assoc", 1, usize::MAX)),
    }
}

fn get(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Map(map, _), MalType::String(key)] => {
            if let Some(value) = map.get(key) {
                Ok(value.clone())
            } else {
                Ok(MalType::Nil)
            }
        }
        [MalType::Nil, _] => Ok(MalType::Nil),
        [_, _] => Err(EvalError::TypeMismatchMultiple(
            "get",
            vec!["map", "string"],
        )),
        _ => Err(EvalError::ArityMismatch("get", 2)),
    }
}

fn contains(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Map(map, _), MalType::String(key)] => {
            Ok(MalType::Boolean(map.contains_key(key)))
        }
        [_, _] => Err(EvalError::TypeMismatchMultiple(
            "contains",
            vec!["map", "string"],
        )),
        _ => Err(EvalError::ArityMismatch("contains", 2)),
    }
}

fn keys(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Map(map, _)] => Ok(MalType::new_list(
            map.keys().map(|s| MalType::String(s.to_string())).collect(),
        )),
        [_] => Err(EvalError::TypeMismatch("keys", "map")),
        _ => Err(EvalError::ArityMismatch("keys", 1)),
    }
}

fn vals(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::Map(map, _)] => {
            Ok(MalType::new_list(map.values().cloned().collect()))
        }
        [_] => Err(EvalError::TypeMismatch("vals", "map")),
        _ => Err(EvalError::ArityMismatch("vals", 1)),
    }
}

fn readline(args: &[MalType]) -> Result<MalType, EvalError> {
    match args {
        [MalType::String(prompt)] => {
            print!("{}", prompt);
            io::stdout().flush().expect("stdout flush failed!");
            let mut line = String::new();
            match io::stdin().read_line(&mut line) {
                Ok(0) => Ok(MalType::Nil),
                Ok(_) => {
                    Ok(MalType::String(line.trim_end_matches('\n').to_string()))
                }
                Err(msg) => panic!("read_line failed: {}", msg),
            }
        }
        [_] => Err(EvalError::TypeMismatch("readline", "string")),
        _ => Err(EvalError::ArityMismatch("readline", 1)),
    }
}

fn not_implemented(_args: &[MalType]) -> Result<MalType, EvalError> {
    Err(EvalError::MalException(MalType::String(
        "not implemented!".to_string(),
    )))
}

mal_predicate!(MalType::Nil, is_nil, "nil?");
mal_predicate!(MalType::Boolean(true), is_true, "true?");
mal_predicate!(MalType::Boolean(false), is_false, "false?");
mal_predicate!(MalType::Symbol(_), is_symbol, "symbol?");
mal_predicate!(MalType::Vector(_, _), is_vector, "vector?");
mal_predicate!(
    MalType::Vector(_, _) | MalType::List(_, _),
    is_sequential,
    "sequential?"
);
mal_predicate!(MalType::Map(_, _), is_map, "map?");
mal_predicate!(
    MalType::String(s) if MalType::is_keyword(s),
    is_keyword,
    "keyword?"
);

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
    ("throw", throw),
    ("apply", apply),
    ("map", map),
    ("nil?", is_nil),
    ("true?", is_true),
    ("false?", is_false),
    ("symbol?", is_symbol),
    ("vector?", is_vector),
    ("map?", is_map),
    ("sequential?", is_sequential),
    ("keyword?", is_keyword),
    ("symbol", symbol),
    ("keyword", keyword),
    ("vector", vector),
    ("hash-map", hash_map),
    ("assoc", assoc),
    ("dissoc", dissoc),
    ("get", get),
    ("contains?", contains),
    ("keys", keys),
    ("vals", vals),
    ("readline", readline),
    ("time-ms", not_implemented),
    ("meta", not_implemented),
    ("with-meta", not_implemented),
    ("fn?", not_implemented),
    ("string?", not_implemented),
    ("number?", not_implemented),
    ("seq", not_implemented),
    ("conj", not_implemented),
];
