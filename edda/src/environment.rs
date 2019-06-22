//! Environment

use std::collections::HashMap;

use crate::interpreter::RuntimeError;
use crate::value::Value;

#[derive(Debug)]
pub struct Environment {
    globals: HashMap<String, Value>,
    locals: Vec<HashMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Environment {
        let locals = vec![HashMap::new()];

        Environment {
            globals: HashMap::new(),
            locals: locals,
        }
    }

    pub fn define_global(&mut self, id: &str, value: Value) -> Result<(), RuntimeError> {
        self.globals.insert(id.to_owned(), value);
        Ok(())
    }

    pub fn push_scope(&mut self) -> () {
        self.locals.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> () {
        match self.locals.pop() {
            None => {
                panic!("Tried to pop local scope, but stack was empty");
            }
            _ => {}
        }
    }

    pub fn define_local(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        let top = self.locals.last_mut().unwrap();

        if top.contains_key(name) {
            return Err(RuntimeError::new(&format!(
                "Variable '{:?}' already declared!",
                name
            )));
        }

        top.insert(name.to_owned(), value);
        Ok(())
    }

    pub fn get_local(&self, name: &str) -> Result<&Value, RuntimeError> {
        for scope in self.locals.iter().rev() {
            match scope.get(name) {
                Some(val) => return Ok(&val),
                None => {}
            }
        }

        Err(RuntimeError::new(&format!(
            "Use of undefined variable {}.",
            name
        )))
    }

    pub fn set_local(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        let top = self.locals.last_mut().unwrap();

        match top.get_mut(name) {
            Some(key) => {
                *key = value;
                Ok(())
            }
            None => Err(RuntimeError::new(&format!(
                "Variable not defined: '{}'",
                name
            ))),
        }
    }
}
