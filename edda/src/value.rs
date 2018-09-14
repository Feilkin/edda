//! Edda value types

use std::rc::Rc;
use std::fmt;

use ast::Expression;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Function(Rc<Function>),
    HostFunction(Rc<HostFunction>),
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Box<Expression>,
    pub arity: usize,
}

impl Function {
    pub fn new(params: Vec<String>, body: Box<Expression>) -> Function {
        Function {
            arity: params.len(),
            params: params,
            body: body,
        }
    }
}

pub struct HostFunction {
    func: Box<Fn(&Vec<Value>) -> Value>,
    arity: usize,
}

impl HostFunction {
    pub fn new<F: 'static + Fn(&Vec<Value>) -> Value>(func: F, arity: usize) -> HostFunction {
        HostFunction {
            func: Box::new(func),
            arity: arity,
        }
    }

    pub fn call(&self, args: &Vec<Value>) -> Value {
        (self.func)(args)
    }
}

impl fmt::Debug for HostFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HostFunction[]")
    }
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
            &Value::Function {..} => write!(f, "Function[]"),
            &Value::HostFunction(_) => write!(f, "HostFunction[]")
        }
    }
}
