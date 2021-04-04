use super::types::MalType;
use crate::types::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvRef = Rc<Env>;

#[derive(Debug)]
pub struct Env {
    outer: Option<EnvRef>,
    data: RefCell<HashMap<String, MalType>>,
}

impl Env {
    pub fn new(outer: Option<EnvRef>) -> Env {
        Env {
            outer,
            data: RefCell::new(HashMap::new()),
        }
    }

    fn find(&self, key: &str) -> Option<&Self> {
        match (self.data.borrow().get(key), &self.outer) {
            (Some(_), _) => Some(self),
            (None, Some(scope)) => scope.find(key),
            (None, None) => None,
        }
    }

    pub fn get(&self, key: &str) -> Result<MalType, EvalError> {
        match self.find(key) {
            Some(env) => Ok(env.data.borrow()[key].clone()),
            None => Err(EvalError::UnknownVariable(key.to_string())),
        }
    }

    pub fn set(&self, key: &str, value: MalType) {
        self.data.borrow_mut().insert(key.to_string(), value);
    }
}
