//! Environment

use std::collections::HashMap;

use interpreter::RuntimeError;
use value::Value;

#[derive(Debug)]
pub struct Environment<'a> {
	values: HashMap<String, Value>,
	enclosing: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
	pub fn new() -> Environment<'a> {
		Environment {
			values: HashMap::new(),
			enclosing: None,
		}
	}

	fn new_with_map(map: HashMap<String, Value>, enclosing: &'a Environment) -> Environment<'a> {
		Environment {
			values: map,
			enclosing: Some(enclosing),
		}
	}

	pub fn define(&'a self, name: &str, value: Value) -> Environment<'a> {
		let mut map = HashMap::new();
		map.insert(name.to_owned(), value);
		Environment::new_with_map(map, &self)
	}

	pub fn get(&self, name: &str) -> Result<Value, RuntimeError> {
		self.values.get(name).map_or(Err(RuntimeError::new("Use of undefined variable.")), |value| { Ok((*value).clone()) })
	}
}