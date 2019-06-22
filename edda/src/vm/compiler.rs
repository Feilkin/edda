//! Edda bytecode compiler

use crate::ast::{Expression, Statement, Literal};
use super::Chunk;
use super::chunk::OpCode;
use super::Value;

pub struct Compiler {

}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn compile(&mut self, ast: &[Statement]) -> Chunk {
        let mut chunk = Chunk::new();

        for statement in ast {
            match *statement {
                Statement::Expression(ref expr) => self.expression::<()>(expr, &mut chunk),
                _ => unimplemented!(),
            }
        }

        chunk.push_op(OpCode::Return, 1);

        chunk
    }

    pub fn expression<'a, T: Value<'a>>(&mut self, expr: &'a Expression, chunk: &mut Chunk) -> () {
        match *expr {
            Expression::Literal(ref lit) => {
                use std::convert::TryInto;

                let val: T = match lit.try_into() {
                    Ok(val) => val,
                    Err(_) => panic!("Invalid literal: {}", lit) // TODO: error handling
                };

                let index = chunk.add_constant(val);

                // TODO: if index > 255, use long format
                chunk.push_op(T::OPCODE, 0);
                chunk.push_byte(index as u8, 0);
            },
            Expression::Grouping(ref expr) => {
                // just call the inner expr lol
                self.expression::<T>(expr, chunk);
            },
            Expression::Binary { ref operator, ref left, ref right } => {
                use crate::token::TokenType;

                self.expression::<f64>(left, chunk);
                self.expression::<f64>(right, chunk);

                // TODO: type stuff

                // TODO: line numbers? token start+end??
                let op = match operator.ttype {
                    TokenType::Plus => OpCode::AddF64,
                    TokenType::Minus => OpCode::SubstractF64,
                    TokenType::Star => OpCode::MultiplyF64,
                    TokenType::Slash => OpCode::DivideF64,
                    _ => panic!("Invalid binary operation!"),
                };

                chunk.push_op(op, 0);
            },
            _ => unimplemented!(),
        }
    }
}