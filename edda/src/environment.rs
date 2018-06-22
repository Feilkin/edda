//! Environment

use std::collections::HashMap;

use interpreter::RuntimeError;
use value::Value;

#[derive(Debug)]
pub struct Environment {
	values: HashMap<String, Value>,
}

impl Environment {
	pub fn new() -> Environment {
		Environment {
			values: HashMap::new(),
		}
	}

	pub fn define(&mut self, name: &str, value: Value) {
		self.values.insert(name.to_owned(), value);
	}

	pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
		self.values.get(name).map_or(Err(RuntimeError::new("Use of undefined variable.")), |value| { Ok((*value).clone()) })
	}
}