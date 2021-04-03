use super::types::MalType;
use crate::types::EvalError;

use std::collections::HashMap;

#[derive(Debug)]
pub struct Env<'e> {
    outer: Option<&'e Env<'e>>,
    data: HashMap<String, MalType>,
}

impl Env<'_> {
    pub fn new<'e>(outer: Option<&'e Env>) -> Env<'e> {
        Env {
            outer,
            data: HashMap::new(),
        }
    }

    fn find(&self, key: &str) -> Option<&Self> {
        match (self.data.get(key), &self.outer) {
            (Some(_), _) => Some(self),
            (None, Some(scope)) => scope.find(key),
            (None, None) => None,
        }
    }

    pub fn get(&self, key: &str) -> Result<&MalType, EvalError> {
        match self.find(key) {
            Some(env) => Ok(&env.data[key]),
            None => Err(EvalError::UnknownVariable(key.to_string())),
        }
    }

    pub fn set(&mut self, key: &str, value: MalType) {
        self.data.insert(key.to_string(), value);
    }
}
