//! Tree-walkin Interpreter for now

use ast::{Statement, Expression, Literal};
use value::Value;
use token::Token;
use environment::Environment;

#[derive(Debug)]
pub struct RuntimeError(&'static str);

impl RuntimeError {
	pub fn new(reason: &'static str) -> RuntimeError {
		RuntimeError(reason)
	}
}

pub struct Interpreter {
	environment: Environment,
}

impl Interpreter {
	pub fn new() -> Interpreter {
		Interpreter {
			environment: Environment::new()
		}
	}

	fn execute(&mut self, stmt: &Statement) -> Result<(), RuntimeError> {
		match stmt {
			&Statement::Print(ref expr) => {
				self.evaluate(expr).and_then(|val| { println!("{}", val); Ok(()) })
			},
			&Statement::Expression(ref expr) => { self.evaluate(expr).and(Ok(())) }
			&Statement::VarDeclaration(ref id, ref initializer) => {
				let mut value = Value::Nil;
				if let Some(expr) = initializer.as_ref() {
					value = self.evaluate(expr).unwrap();
				}

				self.environment.define(id, value);
				Ok(())
			}
		}
	}

	fn evaluate(&mut self, expr: &Expression) -> Result<Value, RuntimeError> {
		match expr {
			&Expression::Literal(ref literal) => {
				match literal {
					&Literal::Number(d) => Ok(Value::Number(d)),
					&Literal::String(ref val) => Ok(Value::String(val.clone())),
					&Literal::Boolean(b) => if b { Ok(Value::True) } else { Ok(Value::False) },
					&Literal::Nil => Ok(Value::Nil),
				}
			},
			&Expression::Variable(ref id) => {
				self.environment.get(id)
			},
			&Expression::Unary{ ref operator, ref expr } => {
				let value = self.evaluate(expr).unwrap();

				match operator {
					&Token::Minus => {
						match value {
							Value::Number(d) => Ok(Value::Number(-d)),
							Value::String(_) => Err(RuntimeError("Cannot negate String.")),
							Value::True | Value::False => Err(RuntimeError("Cannot negate boolean, use ! instead.")),
							Value::Nil => Err(RuntimeError("Cannot negate nil.")),
						}
					},
					&Token::Bang => if self.is_truthy(value) { Ok(Value::False) } else { Ok(Value::True) },
					_ => Err(RuntimeError("invalid unary operator in AST")),
				}
			},
			&Expression::Binary{ ref operator, ref left, ref right } => {
				let left = self.evaluate(left).unwrap();
				let right = self.evaluate(right).unwrap();

				match operator {
					&Token::Minus => {
						match (left, right) {
							(Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
							_ => Err(RuntimeError("Cannot do arithmetic on other than numbers.")),
						}
					},
					&Token::Plus => {
						match (left, right) {
							(Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
							_ => Err(RuntimeError("Cannot do arithmetic on other than numbers.")),
						}
					},
					&Token::Slash => {
						match (left, right) {
							(Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
							_ => Err(RuntimeError("Cannot do arithmetic on other than numbers.")),
						}
					},
					&Token::Star => {
						match (left, right) {
							(Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
							_ => Err(RuntimeError("Cannot do arithmetic on other than numbers.")),
						}
					},
					&Token::Greater => {
						match (left, right) {
							(Value::Number(l), Value::Number(r)) => if l > r { Ok(Value::True) } else { Ok(Value::False) },
							_ => Err(RuntimeError("Cannot do order comparison on other than numbers.")),
						}
					},
					&Token::GreaterEqual => {
						match (left, right) {
							(Value::Number(l), Value::Number(r)) => if l >= r { Ok(Value::True) } else { Ok(Value::False) },
							_ => Err(RuntimeError("Cannot do order comparison on other than numbers.")),
						}
					},
					&Token::Less => {
						match (left, right) {
							(Value::Number(l), Value::Number(r)) => if l < r { Ok(Value::True) } else { Ok(Value::False) },
							_ => Err(RuntimeError("Cannot do order comparison on other than numbers.")),
						}
					},
					&Token::LessEqual => {
						match (left, right) {
							(Value::Number(l), Value::Number(r)) => if l <= r { Ok(Value::True) } else { Ok(Value::False) },
							_ => Err(RuntimeError("Cannot do order comparison on other than numbers.")),
						}
					},
					&Token::BangEqual => if !self.is_equal(&left, &right) { Ok(Value::True) } else { Ok(Value::False) },
					&Token::EqualEqual => if self.is_equal(&left, &right) { Ok(Value::True) } else { Ok(Value::False) },
					_ => Err(RuntimeError("invalid binary operator in AST")),
				}
			},
			&Expression::Grouping(ref expr) => { self.evaluate(expr) },
		}
	}

	fn is_truthy(&self, value: Value) -> bool {
		match value {
			Value::Number(_) => true,
			Value::String(_) => true,
			Value::True => true,
			Value::False => false,
			Value::Nil => false,
		}
	}

	fn is_equal(&self, left: &Value, right: &Value) -> bool {
		match (left, right) {
			(&Value::Nil, &Value::Nil) => true,
			(&Value::Nil, _) => false,
			(&Value::Number(l), &Value::Number(r)) => l == r,
			(&Value::Number(_), _) => false,
			(&Value::String(ref l), &Value::String(ref r)) => l == r,
			(&Value::String(_), _) => false,
			(&Value::True, &Value::True) |
			(&Value::False, &Value::False) => true,
			(_, _) => false,
		}
	}

	pub fn interpret(&mut self, script: &Vec<Statement>) -> Result<Value, RuntimeError> {
		for statement in script {
			if let Err(error) = self.execute(statement) {
				return Err(error)
			}
		}

		Ok(Value::Nil)
	}
}