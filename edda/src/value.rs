//! Edda value types

use ast::Expression;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Function {
        params: Vec<String>,
        body: Box<Expression>,
        arity: usize,
    },
}

// to_string because Rust
// TODO: is this the best way to do this?
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Number(ref val) => write!(f, "{}", val),
            &Value::String(ref val) => write!(f, "{}", val),
            &Value::True => write!(f, "true"),
            &Value::False => write!(f, "false"),
            &Value::Nil => write!(f, "nil"),
            &Value::Function {..} => write!(f, "function[]"),
        }
    }
}
