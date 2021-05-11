use std::fmt::{Display, Write};

use crate::types::MalType;

pub fn pr_str(value: &MalType, print_readably: bool) -> String {
    let mut string = String::new();
    pr_str_internal(value, &mut string, print_readably)
        .expect("Write to string failed!");
    string
}

fn pr_str_internal(
    value: &MalType,
    writer: &mut impl Write,
    print_readably: bool,
) -> Result<(), std::fmt::Error> {
    match value {
        MalType::List(list, _) => {
            write!(
                writer,
                "({})",
                list.iter()
                    .map(|ast| pr_str(ast, print_readably))
                    .collect::<Vec<_>>()
                    .join(" ")
            )?;
        }
        MalType::Vector(vector, _) => {
            write!(
                writer,
                "[{}]",
                vector
                    .iter()
                    .map(|ast| pr_str(ast, print_readably))
                    .collect::<Vec<_>>()
                    .join(" ")
            )?;
        }
        MalType::Map(map, _) => {
            let mut output = vec![];
            for (key, value) in map {
                output.push(pr_str(
                    &MalType::String(key.clone()),
                    print_readably,
                ));
                output.push(pr_str(value, print_readably));
            }
            write!(writer, "{{{}}}", output.join(" "))?;
        }
        MalType::Integer(integer) => {
            write!(writer, "{}", integer)?;
        }
        MalType::Symbol(symbol) => {
            write!(writer, "{}", symbol)?;
        }
        MalType::String(keyword) if MalType::is_keyword(keyword) => {
            write!(writer, ":{}", keyword.chars().skip(1).collect::<String>())?;
        }
        MalType::String(string) if print_readably => {
            write!(writer, "\"")?;
            for c in string.chars() {
                match c {
                    '\n' => write!(writer, "\\n")?,
                    '\t' => write!(writer, "\\t")?,
                    '\r' => write!(writer, "\\r")?,
                    '\\' => write!(writer, "\\\\")?,
                    '\"' => write!(writer, "\\\"")?,
                    c => write!(writer, "{}", c)?,
                }
            }
            write!(writer, "\"")?;
        }
        MalType::String(string) => {
            write!(writer, "{}", string)?;
        }
        MalType::Boolean(true) => {
            write!(writer, "true")?;
        }
        MalType::Boolean(false) => {
            write!(writer, "false")?;
        }
        MalType::Nil => {
            write!(writer, "nil")?;
        }
        MalType::Fn(f, _) => {
            write!(writer, "{}", f)?;
        }
        MalType::FnUser(f, _) => {
            write!(writer, "{}", f)?;
        }
        MalType::Atom(value) => {
            write!(writer, "(atom {})", value)?;
        }
    }

    Ok(())
}

impl Display for MalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        pr_str_internal(self, f, true)
    }
}
