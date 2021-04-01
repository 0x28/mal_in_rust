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
