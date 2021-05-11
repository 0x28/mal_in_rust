use super::types::MalType;
use crate::types::EvalError;

use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvRef = Rc<Env>;

pub struct Env {
    outer: Option<EnvRef>,
    data: Cell<HashMap<String, MalType>>,
}

impl Env {
    pub fn new(outer: Option<EnvRef>) -> Env {
        Env {
            outer,
            data: Cell::new(HashMap::new()),
        }
    }

    pub fn from_bindings(
        outer: Option<EnvRef>,
        binds: Vec<String>,
        exprs: Vec<MalType>,
    ) -> Result<Env, EvalError> {
        match binds.as_slice() {
            [fixed_params @ .., ampersand, var_param] if ampersand == "&" => {
                if exprs.len() < fixed_params.len() {
                    return Err(EvalError::ArityMismatchRange(
                        "lambda",
                        fixed_params.len(),
                        usize::MAX,
                    ));
                }

                let var_args: Vec<_> =
                    exprs.iter().skip(fixed_params.len()).cloned().collect();

                let var_param = var_param.clone();
                let fixed_len = fixed_params.len();

                let mut data: HashMap<_, _> = binds
                    .into_iter()
                    .take(fixed_len)
                    .zip(exprs.into_iter())
                    .collect();

                data.insert(var_param, MalType::new_list(var_args));

                let data = Cell::new(data);

                Ok(Env { outer, data })
            }
            binds if binds.iter().any(|b| b == "&") => {
                Err(EvalError::InvalidFnBinding)
            }
            _ => {
                if binds.len() != exprs.len() {
                    return Err(EvalError::ArityMismatch(
                        "lambda",
                        binds.len(),
                    ));
                }

                let data = binds.into_iter().zip(exprs.into_iter()).collect();
                let data = Cell::new(data);

                Ok(Env { outer, data })
            }
        }
    }

    fn find(&self, key: &str) -> Option<&Self> {
        let data = self.data.take();

        let env = match (data.get(key), &self.outer) {
            (Some(_), _) => Some(self),
            (None, Some(scope)) => scope.find(key),
            (None, None) => None,
        };

        self.data.set(data);

        env
    }

    pub fn get(&self, key: &str) -> Result<MalType, EvalError> {
        match self.find(key) {
            Some(env) => {
                let data = env.data.take();
                let value = Ok(data[key].clone());
                env.data.set(data);
                value
            }
            None => Err(EvalError::UnknownVariable(key.to_string())),
        }
    }

    pub fn set(&self, key: &str, value: MalType) {
        let mut data = self.data.take();
        data.insert(key.to_string(), value);
        self.data.set(data);
    }

    pub fn clear(&self) {
        self.data.take().clear()
    }
}
