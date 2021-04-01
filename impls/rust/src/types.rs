use std::{collections::HashMap, fmt::Display, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
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

pub type Env = HashMap<String, MalType>;

pub enum EvalError {
    UnknownVariable(String),
    TypeMismatch(String, String),
    ArityMismatch(String, usize),
    DivisionByZero,
    ExpectedFunction(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnknownVariable(var) => {
                write!(f, "Undefined variable '{}'", var)
            }
            EvalError::TypeMismatch(func, t) => {
                write!(f, "Function '{}' expects type '{}'", func, t)
            }
            EvalError::ArityMismatch(func, count) => {
                write!(f, "Function '{}' expects {} argument(s)", func, count)
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
        }
    }
}
