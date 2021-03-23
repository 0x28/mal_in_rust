use std::{iter::Peekable, slice::Iter};
use super::types::MalType;

fn escape(escape_char: char) -> Result<char, String> {
    match escape_char {
        't' => Ok('\t'),
        'n' => Ok('\n'),
        'r' => Ok('\r'),
        '\\' => Ok('\\'),
        c => Err(format!("Invalid escape sequence \\{}", c)),
    }
}

fn tokenize(input: &str) -> Result<Vec<String>, String> {
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
                let mut comment = String::new();

                while let Some(&c) = stream.peek() {
                    if c == '\n' {
                        break;
                    } else {
                        comment.push(c);
                    }
                    stream.next();
                }

                token.push(comment);
            }
            ',' => {
                stream.next();
            }
            '"' => {
                let mut closed = false;
                let mut string = String::new();
                stream.next();
                while let Some(&c) = stream.peek() {
                    match c {
                        '\\' => {
                            stream.next();
                            if let Some(&escape_char) = stream.peek() {
                                string.push(escape(escape_char)?);
                                stream.next();
                            } else {
                                return Err(format!(
                                    "EOF after \"{}\\",
                                    string
                                ));
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
                    token.push(string);
                } else {
                    return Err(format!("Undelimited string \"{}...", string));
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
        tokenize(r#" "hello world" "#),
        Ok(vec!["hello world".to_string()])
    );
    assert_eq!(
        tokenize(r#" "bla\tbla" "#),
        Ok(vec!["bla\tbla".to_string()])
    );
    assert_eq!(
        tokenize(r#" "hello world"#),
        Err("Undelimited string \"hello world...".to_string())
    );
    assert_eq!(
        tokenize(r#" (defun inc (x) (+ x 1))"#),
        Ok(vec![
            "(", "defun", "inc", "(", "x", ")", "(", "+", "x", "1", ")", ")"
        ]
        .iter()
        .map(|s| s.to_string())
        .collect())
    );
    assert_eq!(
        tokenize(r#" "test \"#),
        Err("EOF after \"test \\".to_string())
    )
}
