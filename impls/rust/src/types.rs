use std::{collections::HashMap, fmt::Display, rc::Rc};

#[derive(Clone, Debug)]
pub enum MalType {
    Nil,
    List(Vec<MalType>),
    Vector(Vec<MalType>),
    Map(HashMap<String, MalType>),
    Fn(MalFunc),
    Integer(i64),
    Symbol(String),
    String(String),
    Boolean(bool),
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

#[derive(Clone)]
pub struct MalFunc(pub Rc<dyn Fn(&[MalType]) -> Result<MalType, EvalError>>);

impl std::fmt::Display for MalFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function@{:p}>", &self.0)
    }
}

impl std::fmt::Debug for MalFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl PartialEq for MalFunc {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

pub enum EvalError {
    UnknownVariable(String),
    TypeMismatch(String, String),
    ArityMismatch(String, usize),
    ArityMismatchRange(String, usize, usize),
    DivisionByZero,
    ExpectedFunction(String),
    ExpectedSymbol(String),
    InvalidLetBinding,
    InvalidFnBinding,
    EmptyDo,
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnknownVariable(var) => {
                write!(f, "Variable '{}' not found", var)
            }
            EvalError::TypeMismatch(func, t) => {
                write!(f, "Function '{}' expects type '{}'", func, t)
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
            EvalError::EmptyDo => {
                write!(f, "do should at least contain one expression")
            }
        }
    }
}
