use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum MalType {
    Nil,
    List(Vec<MalType>),
    Vector(Vec<MalType>),
    Map(HashMap<String, MalType>),
    Integer(i64),
    Symbol(String),
    String(String),
    Boolean(bool),
}
