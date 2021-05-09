use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::io;
use std::rc::Rc;

use crate::{env::EnvRef, reader::ReaderError};

#[derive(Clone, Debug)]
pub enum MalType {
    Nil,
    List(Vec<MalType>),
    Vector(Vec<MalType>),
    Map(HashMap<String, MalType>),
    Fn(InternalFn),
    FnUser(Rc<UserFn>),
    Integer(i64),
    Symbol(String),
    String(String),
    Boolean(bool),
    Atom(MalAtom),
}

impl MalType {
    pub const KEYWORD_SYMBOL: char = '\u{29E}';

    pub fn is_keyword(value: &str) -> bool {
        value.starts_with(MalType::KEYWORD_SYMBOL)
    }
}

impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MalType::Nil, MalType::Nil) => true,
            (MalType::List(l), MalType::List(r)) => l == r,
            (MalType::Vector(l), MalType::Vector(r)) => l == r,
            (MalType::Map(l), MalType::Map(r)) => l == r,
            (MalType::Fn(l), MalType::Fn(r)) => l == r,
            (MalType::Integer(l), MalType::Integer(r)) => l == r,
            (MalType::Symbol(l), MalType::Symbol(r)) => l == r,
            (MalType::String(l), MalType::String(r)) => l == r,
            (MalType::Boolean(l), MalType::Boolean(r)) => l == r,
            (MalType::List(l), MalType::Vector(r)) => l == r,
            (MalType::Vector(l), MalType::List(r)) => l == r,
            _ => false,
        }
    }
}

impl Default for MalType {
    fn default() -> Self {
        MalType::Nil
    }
}

#[derive(Clone)]
pub struct UserFn {
    pub ast: MalType,
    pub params: Vec<String>,
    pub env: EnvRef,
    pub fun: InternalFn,
    is_macro: Cell<bool>,
}

impl UserFn {
    pub fn new(
        ast: MalType,
        params: Vec<String>,
        env: EnvRef,
        fun: InternalFn,
    ) -> UserFn {
        UserFn {
            ast,
            params,
            env,
            fun,
            is_macro: Cell::new(false),
        }
    }

    pub fn mark_as_macro(&self) {
        self.is_macro.swap(&Cell::new(true));
    }

    pub fn is_macro(&self) -> bool {
        self.is_macro.get()
    }
}

impl std::fmt::Debug for UserFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fun)
    }
}

impl std::fmt::Display for UserFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fun)
    }
}

#[derive(Clone)]
pub struct InternalFn(pub Rc<dyn Fn(&[MalType]) -> Result<MalType, EvalError>>);

impl std::fmt::Display for InternalFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function@{:p}>", &self.0)
    }
}

impl std::fmt::Debug for InternalFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl PartialEq for InternalFn {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

pub struct MalAtom {
    value: Rc<Cell<MalType>>,
}

impl MalAtom {
    pub fn new(value: MalType) -> MalAtom {
        MalAtom {
            value: Rc::new(Cell::new(value)),
        }
    }

    pub fn deref(&self) -> MalType {
        let value = self.value.take();
        self.value.set(value.clone());
        value
    }

    pub fn reset(&self, value: MalType) {
        self.value.set(value);
    }
}

impl Display for MalAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = self.value.take();
        let result = write!(f, "{}", value);
        self.value.set(value);

        result
    }
}

impl Debug for MalAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Clone for MalAtom {
    fn clone(&self) -> Self {
        MalAtom {
            value: Rc::clone(&self.value),
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    UnknownVariable(String),
    TypeMismatch(&'static str, &'static str),
    TypeMismatchMultiple(&'static str, Vec<&'static str>),
    ArityMismatch(&'static str, usize),
    ArityMismatchRange(&'static str, usize, usize),
    ArityMismatchEven(&'static str),
    ArityMismatchOdd(&'static str),
    DivisionByZero,
    ExpectedFunction(String),
    ExpectedSymbol(String),
    InvalidLetBinding,
    InvalidFnBinding,
    InvalidCatchBinding,
    EmptyDo,
    ReaderError(ReaderError),
    InOutError(io::Error),
    InvalidIndex(usize, usize),
    MalException(MalType),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnknownVariable(var) => {
                write!(f, "'{}' not found", var)
            }
            EvalError::TypeMismatch(func, t) => {
                write!(f, "Function '{}' expects type '{}'", func, t)
            }
            EvalError::TypeMismatchMultiple(func, types) => {
                write!(
                    f,
                    "Function '{}' expects the argument types {}",
                    func,
                    types
                        .iter()
                        .map(|s| format!("'{}'", s))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            EvalError::ArityMismatch(func, count) => {
                write!(f, "Function '{}' expects {} argument(s)", func, count)
            }
            EvalError::ArityMismatchRange(func, min, max) => {
                if *max == usize::MAX {
                    write!(
                        f,
                        "Function '{}' expects at least {} argument(s)",
                        func, min
                    )
                } else {
                    write!(
                        f,
                        "Function '{}' expects between {} and {} argument(s)",
                        func, min, max
                    )
                }
            }
            EvalError::ArityMismatchEven(func) => {
                write!(
                    f,
                    "Function '{}' expects an even number of argument(s)",
                    func
                )
            }
            EvalError::ArityMismatchOdd(func) => {
                write!(
                    f,
                    "Function '{}' expects an odd number of argument(s)",
                    func
                )
            }
            EvalError::DivisionByZero => {
                write!(f, "Division by zero")
            }
            EvalError::ExpectedFunction(actual) => {
                write!(
                    f,
                    "Expected function in call position found '{}'",
                    actual
                )
            }
            EvalError::ExpectedSymbol(actual) => {
                write!(f, "Expected symbol found '{}'", actual)
            }
            EvalError::InvalidLetBinding => {
                write!(
                    f,
                    "let* binding not of the form \
                     (let* (<symbol> <expr> ...) <expr>)"
                )
            }
            EvalError::InvalidFnBinding => {
                write!(
                    f,
                    "fn* binding not of the form \
                     (fn* (<symbol> ...[& <symbol>]) <expr>)"
                )
            }
            EvalError::InvalidCatchBinding => {
                write!(
                    f,
                    "catch* binding not of the form \
                     (catch* <symbol> <expr>)"
                )
            }
            EvalError::EmptyDo => {
                write!(f, "do should at least contain one expression")
            }
            EvalError::ReaderError(err) => {
                write!(f, "{}", err)
            }
            EvalError::InOutError(err) => {
                write!(f, "{}", err)
            }
            EvalError::InvalidIndex(size, idx) => {
                write!(
                    f,
                    "Index '{}' out of bounds in collection of size '{}'",
                    idx, size
                )
            }
            EvalError::MalException(exc) => {
                write!(f, "Uncaught exception '{}'", exc)
            }
        }
    }
}
