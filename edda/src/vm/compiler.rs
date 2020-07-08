//! Standard bytecode compiler

use thiserror::Error as ThisError;

use crate::token::TokenType;
use crate::ast::expressions::{Expression, Addition, Binary};
use crate::vm::bytecode::{Chunk, OpCode};
use crate::Error;

#[derive(ThisError, Debug)]
#[error("Failed to compile")]
pub struct CompilerError {}

impl From<CompilerError> for Error {
    fn from(d: CompilerError) -> Self {
        Error::CompilerError(d)
    }
}

type CompilerResult = Result<Chunk, CompilerError>;

pub fn compile(ast: Expression, mut chunk: Chunk) -> CompilerResult {
    match ast {
        Expression::Literal(lit) => {
            let val: i32 = lit.token().text.parse().expect("failed to parse int :(");

            chunk.push_op(OpCode::ConstantI32);
            chunk.push_value(val.to_le_bytes());

            Ok(chunk)
        },
        Expression::Addition(expr) => {
            let Binary {
                lhs,
                operator,
                rhs,
            } = expr.0;

            chunk = compile(*lhs, chunk)?;
            chunk = compile(*rhs, chunk)?;

            let op = match operator.t_type {
                TokenType::Plus => OpCode::AddI32,
                TokenType::Minus => unimplemented!(),
                _ => unreachable!(),
            };

            chunk.push_op(op);

            Ok(chunk)
        }
        _ => unimplemented!()
    }
}