//! Tree-walkin Interpreter for now

use std::rc::Rc;
use crate::ast::{Expression, Literal, Statement};
use crate::environment::Environment;
use crate::token::TokenType;
use crate::value::{Value, Function, HostFunction};

#[derive(Debug)]
pub struct RuntimeError(String);

impl RuntimeError {
    pub fn new(reason: &str) -> RuntimeError {
        RuntimeError(reason.to_owned())
    }
}

pub trait Printer {
    fn print(&mut self, text: &str) -> ();
}

pub struct DefaultPrinter {}
impl Printer for DefaultPrinter {
    fn print(&mut self, text: &str) {
        print!("{}", text);
    }
}

pub struct Interpreter<P: Printer> {
    pub environment: Environment,
    pub printer: P
}

impl Interpreter<DefaultPrinter> {
    pub fn new() -> Interpreter<DefaultPrinter> {
        Interpreter {
            environment: Environment::new(),
            printer: DefaultPrinter {},
        }
    }
}

impl<P: Printer> Interpreter<P> {
    pub fn new_with_printer(printer: P) -> Interpreter<P> {
        Interpreter {
            environment: Environment::new(),
            printer: printer,
        }
    }

    fn execute(&mut self, stmt: &Statement) -> Result<(), RuntimeError> {
        match stmt {
            &Statement::For(ref params, ref iter, ref body) => {
                let iter_f = match self.evaluate(iter)? {
                    Value::Function(ref func) => { Rc::clone(func) },
                    _ => panic!("lol"),
                };

                let arg_vec = Vec::new();
                let mut initial = self.call_function(Rc::clone(&iter_f), &arg_vec)?;

                while match initial { Value::Nil => false, _ => true } {
                    self.environment.push_scope();
                    self.environment.define_local(&params[0], initial)?;
                    self.execute(body)?;
                    self.environment.pop_scope();

                    initial = self.call_function(Rc::clone(&iter_f), &arg_vec)?;
                }

                Ok(())
            },
            &Statement::If(ref cond, ref then, ref else_body) => {
                if is_truthy(self.evaluate(cond)?) {
                    self.environment.push_scope();
                    let ret = self.execute(then)?;
                    self.environment.pop_scope();

                    Ok(ret)
                } else {
                    if let Some(b) = else_body {
                        self.environment.push_scope();
                        let ret = self.execute(b)?;
                        self.environment.pop_scope();
                        Ok(ret)
                    } else {
                        Ok(())
                    }
                }
            }
            &Statement::Print(ref expr) => self.evaluate(expr).and_then(|val| {
                self.printer.print(&format!("{}\n", val));
                Ok(())
            }),
            &Statement::Expression(ref expr) => self.evaluate(expr).and(Ok(())),
            &Statement::VarDeclaration(ref id, ref initializer) => {
                let mut value = Value::Nil;
                if let Some(expr) = initializer.as_ref() {
                    value = self.evaluate(expr)?;
                }

                self.environment.define_local(id, value).unwrap();
                Ok(())
            }
            &Statement::GlobalDeclaration(ref id, ref initializer) => {
                let value = self.evaluate(initializer)?;
                self.environment.define_global(id, value)
            }
            &Statement::BlockStatement(ref statements) => {
                for statement in statements {
                    self.execute(statement).unwrap();
                }
                Ok(())
            }
        }
    }

    fn call_function(&mut self, func: Rc<Function>, args: &Vec<Expression>) -> Result<Value, RuntimeError> {
        self.environment.push_scope();

        if args.len() != func.arity {
            return Err(RuntimeError(format!("Wrong number of arguments, got {}, expected {}", args.len(), func.arity)))
        }

        let mut i = 0;
        for arg in args.into_iter() {
            let id = &func.params[i];
            let val = self.evaluate(arg)?;
            self.environment.define_local(id, val)?;

            i += 1;
        }

        let res = self.evaluate(&func.body)?;
        self.environment.pop_scope();

        Ok(res)
    }

    fn call_host_function(&mut self, host_func: &Rc<HostFunction>, args: &Vec<Expression>) -> Result<Value, RuntimeError> {
        let args_evaluated = args.into_iter().map(|arg| { self.evaluate(&arg).unwrap() }).collect();

        Ok(host_func.call(&args_evaluated))
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<Value, RuntimeError> {
        match expr {
            &Expression::If(ref cond, ref then, ref else_body) => {
                if is_truthy(self.evaluate(cond)?) {
                    self.evaluate(then)
                } else {
                    if let Some(b) = else_body {
                        self.evaluate(b)
                    } else {
                        Ok(Value::Nil)
                    }
                }
            },
            &Expression::FunctionDeclaration(ref params, ref body) => {
                Ok(Value::Function(Rc::new(Function::new(params.clone(), body.clone()))))
            },
            &Expression::FunctionCall(ref callee, ref args) => {
                // branch out to handle different types of functions
                match self.evaluate(callee)? {
                    Value::Function(ref func) => self.call_function(Rc::clone(func), args),
                    Value::HostFunction(host_func) => self.call_host_function(&host_func, args),
                    other @ _ => {
                        return Err(RuntimeError(format!("Attempting to call a {:?}!", other)))
                    }
                }
            },
            &Expression::BlockExpression(ref statements, ref ret) => {
                self.environment.push_scope();
                for statement in statements {
                    self.execute(statement).unwrap();
                }

                let ret_val = self.evaluate(ret)?;
                self.environment.pop_scope();
                Ok(ret_val)
            }
            &Expression::Literal(ref literal) => match literal {
                &Literal::Number(d) => Ok(Value::Number(d)),
                &Literal::String(ref val) => Ok(Value::String(val.clone())),
                &Literal::Boolean(b) => if b {
                    Ok(Value::True)
                } else {
                    Ok(Value::False)
                },
                &Literal::Nil => Ok(Value::Nil),
            },
            &Expression::Variable(ref id) => self.environment
                .get_local(id)
                .and_then(|val| Ok(val.clone())),
            &Expression::Unary {
                ref operator,
                ref expr,
            } => {
                let value = self.evaluate(expr).unwrap();

                match operator.ttype {
                    TokenType::Minus => match value {
                        Value::Number(d) => Ok(Value::Number(-d)),
                        Value::String(_) => Err(RuntimeError::new("Cannot negate String.")),
                        Value::True | Value::False => {
                            Err(RuntimeError::new("Cannot negate boolean, use ! instead."))
                        }
                        Value::Nil => Err(RuntimeError::new("Cannot negate nil.")),
                        Value::Function {..} | Value::HostFunction(..) => Err(RuntimeError::new("Cannot negate function.")),
                    },
                    TokenType::Bang => if is_truthy(value) {
                        Ok(Value::False)
                    } else {
                        Ok(Value::True)
                    },
                    _ => Err(RuntimeError::new("invalid unary operator in AST")),
                }
            }
            &Expression::Binary {
                ref operator,
                ref left,
                ref right,
            } => {
                let left = self.evaluate(left).unwrap();
                let right = self.evaluate(right).unwrap();

                match operator.ttype {
                    TokenType::Minus => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                        _ => Err(RuntimeError::new(
                            "Cannot do arithmetic on other than numbers.",
                        )),
                    },
                    TokenType::Plus => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                        _ => Err(RuntimeError::new(
                            "Cannot do arithmetic on other than numbers.",
                        )),
                    },
                    TokenType::Slash => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                        _ => Err(RuntimeError::new(
                            "Cannot do arithmetic on other than numbers.",
                        )),
                    },
                    TokenType::Star => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                        _ => Err(RuntimeError::new(
                            "Cannot do arithmetic on other than numbers.",
                        )),
                    },
                    TokenType::Greater => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => if l > r {
                            Ok(Value::True)
                        } else {
                            Ok(Value::False)
                        },
                        _ => Err(RuntimeError::new(
                            "Cannot do order comparison on other than numbers.",
                        )),
                    },
                    TokenType::GreaterEqual => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => if l >= r {
                            Ok(Value::True)
                        } else {
                            Ok(Value::False)
                        },
                        _ => Err(RuntimeError::new(
                            "Cannot do order comparison on other than numbers.",
                        )),
                    },
                    TokenType::Less => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => if l < r {
                            Ok(Value::True)
                        } else {
                            Ok(Value::False)
                        },
                        _ => Err(RuntimeError::new(
                            "Cannot do order comparison on other than numbers.",
                        )),
                    },
                    TokenType::LessEqual => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => if l <= r {
                            Ok(Value::True)
                        } else {
                            Ok(Value::False)
                        },
                        _ => Err(RuntimeError::new(
                            "Cannot do order comparison on other than numbers.",
                        )),
                    },
                    TokenType::BangEqual => if !is_equal(&left, &right) {
                        Ok(Value::True)
                    } else {
                        Ok(Value::False)
                    },
                    TokenType::EqualEqual => if is_equal(&left, &right) {
                        Ok(Value::True)
                    } else {
                        Ok(Value::False)
                    },
                    _ => Err(RuntimeError::new("invalid binary operator in AST")),
                }
            }
            &Expression::Grouping(ref expr) => self.evaluate(expr),
        }
    }

    pub fn interpret(&mut self, script: &Vec<Statement>) -> Result<Value, RuntimeError> {
        for statement in script {
            if let Err(error) = self.execute(statement) {
                return Err(error);
            }
        }

        Ok(Value::Nil)
    }
}

fn is_truthy(value: Value) -> bool {
    match value {
        Value::Number(_) => true,
        Value::String(_) => true,
        Value::True => true,
        Value::False => false,
        Value::Nil => false,
        Value::Function{..} | Value::HostFunction(_) => true,
    }
}

fn is_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (&Value::Nil, &Value::Nil) => true,
        (&Value::Nil, _) => false,
        (&Value::Number(l), &Value::Number(r)) => l == r,
        (&Value::Number(_), _) => false,
        (&Value::String(ref l), &Value::String(ref r)) => l == r,
        (&Value::String(_), _) => false,
        (&Value::True, &Value::True) | (&Value::False, &Value::False) => true,
        (_, _) => false,
    }
}