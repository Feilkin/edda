//! Standard bytecode compiler

use thiserror::Error as ThisError;

use crate::ast::expressions::{Addition, Binary, Expression, Grouping};
use crate::ast::statements::Statement;
use crate::token::{Token, TokenType};
use crate::typer::{Type, TypeError, Typer};
use crate::vm::bytecode::{Chunk, OpCode};
use crate::Error;

#[derive(ThisError, Debug)]
pub enum CompilerError {
    #[error("compile error: {0}")]
    TypeError(#[from] TypeError),
}

impl From<CompilerError> for Error {
    fn from(d: CompilerError) -> Self {
        Error::CompilerError(d)
    }
}

type CompilerResult = Result<Chunk, CompilerError>;

pub struct Local<'s> {
    name: Token<'s>,
    depth: usize,
}

pub struct Compiler<'s> {
    locals: Vec<Local<'s>>,
    scope_depth: usize,
}

impl<'s> Compiler<'s> {
    pub fn new() -> Compiler<'s> {
        Compiler {
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    pub fn compile(&self, script: Vec<Statement>, mut chunk: Chunk) -> CompilerResult {
        for stmt in script {
            chunk = self.compile_statement(stmt, chunk)?;
        }

        Ok(chunk)
    }

    pub fn compile_statement(&self, stmt: Statement, mut chunk: Chunk) -> CompilerResult {
        match stmt {
            Statement::Return(ret) => {
                if let Some(expr) = ret.value {
                    chunk = self.compile_expr(*expr, chunk)?;
                }

                chunk.push_op(OpCode::Return);
                Ok(chunk)
            }
            _ => unimplemented!(),
        }
    }

    pub fn compile_expr(&self, ast: Expression, mut chunk: Chunk) -> CompilerResult {
        let typer = Typer::new();

        match ast {
            Expression::Literal(lit) => {
                let val: i32 = lit.token().text.parse().expect("failed to parse int :(");

                chunk.push_op(OpCode::ConstantI32);
                chunk.push_value(val.to_le_bytes());

                Ok(chunk)
            }
            Expression::Addition(expr) => {
                let Binary { lhs, operator, rhs } = expr.0;

                chunk = self.compile_expr(*lhs, chunk)?;
                chunk = self.compile_expr(*rhs, chunk)?;

                let op = match operator.t_type {
                    TokenType::Plus => OpCode::AddI32,
                    TokenType::Minus => OpCode::SubI32,
                    _ => unreachable!(),
                };

                chunk.push_op(op);

                Ok(chunk)
            }
            Expression::Multiplication(expr) => {
                let Binary { lhs, operator, rhs } = expr.0;

                chunk = self.compile_expr(*lhs, chunk)?;
                chunk = self.compile_expr(*rhs, chunk)?;

                let op = match operator.t_type {
                    TokenType::Star => OpCode::MulI32,
                    TokenType::Slash => OpCode::DivI32,
                    _ => unreachable!(),
                };

                chunk.push_op(op);

                Ok(chunk)
            }
            Expression::Group(expr) => {
                let Grouping { inner } = expr;

                self.compile_expr(*inner, chunk)
            }
            Expression::If(if_expr) => {
                let cond_type = typer.infer_type(if_expr.condition.as_ref())?;

                if cond_type != Type::Boolean {
                    return Err(TypeError {
                        err: cond_type,
                        expected: Type::Boolean,
                    }
                    .into());
                }

                let body_type = typer.infer_type(if_expr.body.as_ref())?;
                let else_type = typer.infer_type(if_expr.else_body.as_ref())?;

                if body_type != else_type {
                    Err(TypeError {
                        err: else_type,
                        expected: body_type,
                    }
                    .into())
                } else {
                    chunk = self.compile_expr(*if_expr.condition, chunk)?;
                    chunk.push_op(OpCode::JumpIfFalse);

                    // we need to backpatch jump location
                    let then_jump = chunk.head();
                    chunk.push_value(0xFF_FF_u16.to_le_bytes());
                    chunk = self.compile_expr(*if_expr.body, chunk)?;

                    // if expressions always have else, so push jump and
                    chunk.push_op(OpCode::Jump);
                    let else_jump = chunk.code.len();
                    chunk.push_value(0xFF_FF_u16.to_le_bytes());

                    // backpatch then jump
                    let then_relative = chunk.head_short() - then_jump as u16;
                    chunk.set_value(then_relative.to_le_bytes(), then_jump);

                    chunk = self.compile_expr(*if_expr.else_body, chunk)?;

                    // backpatch else jump
                    let else_relative = chunk.head_short() - else_jump as u16;
                    chunk.set_value(else_relative.to_le_bytes(), else_jump);

                    Ok(chunk)
                }
            }
            Expression::Equality(expr) => {
                let Binary { lhs, operator, rhs } = expr.0;

                chunk = self.compile_expr(*lhs, chunk)?;
                chunk = self.compile_expr(*rhs, chunk)?;

                chunk.push_op(OpCode::EqualI32);

                Ok(chunk)
            }
            Expression::Comparison(expr) => {
                let Binary { lhs, operator, rhs } = expr.0;

                // swap lhs and rhs to save opcodes :)
                let (lhs, rhs) = match operator.t_type {
                    TokenType::Greater | TokenType::GreaterEqual => (rhs, lhs),
                    _ => (lhs, rhs),
                };

                chunk = self.compile_expr(*lhs, chunk)?;
                chunk = self.compile_expr(*rhs, chunk)?;

                let op = match operator.t_type {
                    TokenType::Less | TokenType::GreaterEqual => OpCode::LessI32,
                    TokenType::LessEqual | TokenType::Greater => OpCode::LessEqualI32,
                    _ => unreachable!(),
                };

                chunk.push_op(op);

                Ok(chunk)
            }
            _ => unimplemented!(),
        }
    }
}
