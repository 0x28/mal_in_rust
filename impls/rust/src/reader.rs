use super::types::MalType;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::iter::Peekable;
use std::slice::Iter;

type Reader<'a> = Peekable<Iter<'a, String>>;

#[derive(Debug, PartialEq)]
pub enum ReaderError {
    Expected(String, String),
    UnexpectedEof,
    InvalidEscape(char),
    UnbalancedString(String),
    InvalidInteger(String),
    InvalidMapKey,
}

impl Display for ReaderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReaderError::Expected(expected, actual) => {
                write!(f, "Expected {} found {}.", expected, actual)?;
            }
            ReaderError::InvalidEscape(c) => {
                write!(f, "Invalid escape sequence '\\{}'.", c)?;
            }
            ReaderError::UnbalancedString(string) => {
                write!(f, "Found unbalanced string {}...", string)?;
            }
            ReaderError::UnexpectedEof => {
                write!(f, "Unexpected EOF.")?;
            }
            ReaderError::InvalidInteger(integer) => {
                write!(f, "Invalid integer '{}'.", integer)?;
            }
            ReaderError::InvalidMapKey => {
                write!(f, "Only strings and keywords can be used in maps.")?;
            }
        }

        Ok(())
    }
}

impl Error for ReaderError {}

pub fn read_str(input: &str) -> Result<MalType, ReaderError> {
    let token = tokenize(input)?;
    let mut reader = token.iter().peekable();
    read_form(&mut reader)
}

fn read_form(reader: &mut Reader) -> Result<MalType, ReaderError> {
    if let Some(lookahead) = reader.peek() {
        match lookahead.as_ref() {
            "(" => read_list(reader),
            "[" => read_vector(reader),
            "{" => read_map(reader),
            "'" => wrap_form("quote", reader),
            "`" => wrap_form("quasiquote", reader),
            "~" => wrap_form("unquote", reader),
            "~@" => wrap_form("splice-unquote", reader),
            "@" => wrap_form("deref", reader),
            "^" => wrap_with_meta(reader),
            _ => read_atom(reader),
        }
    } else {
        Err(ReaderError::UnexpectedEof)
    }
}

fn wrap_form(
    operation: &str,
    reader: &mut Reader,
) -> Result<MalType, ReaderError> {
    let _ = reader.next();
    let form = read_form(reader)?;

    let sexpr =
        MalType::List(vec![MalType::Symbol(operation.to_string()), form]);

    Ok(sexpr)
}

fn wrap_with_meta(reader: &mut Reader) -> Result<MalType, ReaderError> {
    let _ = reader.next();
    let second_arg = read_form(reader)?;
    let first_arg = read_form(reader)?;

    let with_meta = MalType::List(vec![
        MalType::Symbol("with-meta".to_string()),
        first_arg,
        second_arg,
    ]);

    Ok(with_meta)
}

fn read_atom(reader: &mut Reader) -> Result<MalType, ReaderError> {
    if let Some(token) = reader.next() {
        match token.chars().next().expect("Empty token") {
            '0'..='9' => Ok(MalType::Integer(token.parse().map_err(|_| {
                ReaderError::InvalidInteger(token.to_string())
            })?)),
            '"' => Ok(MalType::String(token[1..token.len() - 1].to_string())),
            ':' => Ok(MalType::String(format!("\u{029E}{}", &token[1..]))),
            '-' | '+'
                if token.len() > 1
                    && token.chars().skip(1).all(|c| c.is_digit(10)) =>
            {
                Ok(MalType::Integer(token.parse().map_err(|_| {
                    ReaderError::InvalidInteger(token.to_string())
                })?))
            }
            _ => match token.as_ref() {
                "true" => Ok(MalType::Boolean(true)),
                "false" => Ok(MalType::Boolean(false)),
                "nil" => Ok(MalType::Nil),
                _ => Ok(MalType::Symbol(token.to_string())),
            },
        }
    } else {
        Err(ReaderError::UnexpectedEof)
    }
}

fn read_list(reader: &mut Reader) -> Result<MalType, ReaderError> {
    Ok(MalType::List(read_sequence(reader, ("(", ")"))?))
}

fn read_vector(reader: &mut Reader) -> Result<MalType, ReaderError> {
    Ok(MalType::Vector(read_sequence(reader, ("[", "]"))?))
}

fn read_sequence(
    reader: &mut Reader,
    delimiter: (&str, &str),
) -> Result<Vec<MalType>, ReaderError> {
    let mut sequence = vec![];
    expect(reader, delimiter.0)?;
    while let Some(lookahead) = reader.peek() {
        if *lookahead == delimiter.1 {
            break;
        } else {
            sequence.push(read_form(reader)?)
        }
    }
    expect(reader, delimiter.1)?;

    Ok(sequence)
}

fn read_map(reader: &mut Reader) -> Result<MalType, ReaderError> {
    let mut map = HashMap::new();

    expect(reader, "{")?;
    while let Some(lookahead) = reader.peek() {
        match lookahead.as_ref() {
            "}" => break,
            _ => {
                if let MalType::String(key) = read_form(reader)? {
                    map.insert(key, read_form(reader)?);
                } else {
                    return Err(ReaderError::InvalidMapKey);
                }
            }
        }
    }
    expect(reader, "}")?;

    Ok(MalType::Map(map))
}

fn expect(
    reader: &mut Reader,
    expected_token: &str,
) -> Result<(), ReaderError> {
    let next = reader.next();
    match next {
        Some(actual_token) if *actual_token == expected_token => Ok(()),
        Some(actual_token) => Err(ReaderError::Expected(
            expected_token.to_string(),
            actual_token.to_string(),
        )),
        None => Err(ReaderError::Expected(
            expected_token.to_string(),
            "EOF".to_string(),
        )),
    }
}

fn escape(escape_char: char) -> Result<char, ReaderError> {
    match escape_char {
        't' => Ok('\t'),
        'n' => Ok('\n'),
        'r' => Ok('\r'),
        '\\' => Ok('\\'),
        '\"' => Ok('"'),
        c => Err(ReaderError::InvalidEscape(c)),
    }
}

fn tokenize(input: &str) -> Result<Vec<String>, ReaderError> {
    let mut stream = input.chars().peekable();
    let mut token = vec![];

    while let Some(c) = stream.peek() {
        match c {
            '~' => {
                stream.next();
                if let Some('@') = stream.peek() {
                    token.push("~@".to_string());
                    stream.next();
                } else {
                    token.push("~".to_string());
                }
            }
            '[' | ']' | '{' | '}' | '(' | ')' | '\'' | '`' | '^' | '@' => {
                token.push(c.to_string());
                stream.next();
            }
            ';' => {
                while let Some(&c) = stream.peek() {
                    if c == '\n' {
                        break;
                    } else {
                        stream.next();
                    }
                }
            }
            ',' => {
                stream.next();
            }
            '"' => {
                let mut closed = false;
                let mut string = String::from("\"");
                stream.next();
                while let Some(&c) = stream.peek() {
                    match c {
                        '\\' => {
                            stream.next();
                            if let Some(&escape_char) = stream.peek() {
                                string.push(escape(escape_char)?);
                                stream.next();
                            } else {
                                return Err(ReaderError::UnexpectedEof);
                            }
                        }
                        '"' => {
                            closed = true;
                            stream.next();
                            break;
                        }
                        c => {
                            string.push(c);
                            stream.next();
                        }
                    }
                }
                if closed {
                    string.push('\"');
                    token.push(string);
                } else {
                    return Err(ReaderError::UnbalancedString(string));
                }
            }
            c if c.is_whitespace() => {
                stream.next();
            }
            _ => {
                let mut other = String::new();
                while let Some(&c) = stream.peek() {
                    if c.is_whitespace() || "[]{}('\"`,;)".contains(c) {
                        break;
                    } else {
                        other.push(c);
                    }

                    stream.next();
                }
                token.push(other);
            }
        }
    }

    Ok(token)
}

#[test]
fn test_tokenize() {
    assert_eq!(
        tokenize("(+ 1 2 3)"),
        Ok(vec!["(", "+", "1", "2", "3", ")"]
            .iter()
            .map(|s| s.to_string())
            .collect())
    );
    assert_eq!(
        tokenize("~a"),
        Ok(vec!["~", "a"].iter().map(|s| s.to_string()).collect())
    );
    assert_eq!(
        tokenize(r#" "hello world" "#),
        Ok(vec!["\"hello world\"".to_string()])
    );
    assert_eq!(
        tokenize(r#" "bla\tbla" "#),
        Ok(vec!["\"bla\tbla\"".to_string()])
    );
    assert_eq!(
        tokenize(r#" "hello world"#),
        Err(ReaderError::UnbalancedString("\"hello world".to_string()))
    );
    assert_eq!(
        tokenize("(defun inc (x) (+ x 1))"),
        Ok(vec![
            "(", "defun", "inc", "(", "x", ")", "(", "+", "x", "1", ")", ")"
        ]
        .iter()
        .map(|s| s.to_string())
        .collect())
    );
    assert_eq!(tokenize(r#" "test \"#), Err(ReaderError::UnexpectedEof))
}

#[test]
fn test_read_str() {
    assert_eq!(read_str("()"), Ok(MalType::List(vec![])));
    assert_eq!(
        read_str("(())"),
        Ok(MalType::List(vec![MalType::List(vec![])]))
    );
    assert_eq!(
        read_str("((()))"),
        Ok(MalType::List(vec![MalType::List(vec![MalType::List(
            vec![]
        )])]))
    );
    assert_eq!(
        read_str("[1 2 3]"),
        Ok(MalType::Vector(vec![
            MalType::Integer(1),
            MalType::Integer(2),
            MalType::Integer(3),
        ]))
    );
    let mut map = HashMap::new();
    map.insert("a".to_owned(), MalType::Integer(100));
    assert_eq!(read_str("{\"a\" 100}"), Ok(MalType::Map(map)));
    assert_eq!(read_str("-100"), Ok(MalType::Integer(-100)));
    assert_eq!(read_str("+100"), Ok(MalType::Integer(100)));
    assert_eq!(read_str("true"), Ok(MalType::Boolean(true)));
    assert_eq!(read_str("false"), Ok(MalType::Boolean(false)));
    assert_eq!(
        read_str("(1 2 3 42 64 128)"),
        Ok(MalType::List(vec![
            MalType::Integer(1),
            MalType::Integer(2),
            MalType::Integer(3),
            MalType::Integer(42),
            MalType::Integer(64),
            MalType::Integer(128),
        ]))
    );
    assert_eq!(
        read_str("(defun inc (x) (+ x 1))"),
        Ok(MalType::List(vec![
            MalType::Symbol("defun".to_string()),
            MalType::Symbol("inc".to_string()),
            MalType::List(vec![MalType::Symbol("x".to_string())]),
            MalType::List(vec![
                MalType::Symbol("+".to_string()),
                MalType::Symbol("x".to_string()),
                MalType::Integer(1),
            ])
        ]))
    );
}
