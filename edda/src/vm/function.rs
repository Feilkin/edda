//! Static functions :)

use crate::typer::{FromStack, Type};
use crate::{Chunk, OpCode};
use itertools::Either;
use std::convert::TryFrom;
use std::marker::PhantomData;
use std::ops::Range;
use std::rc::Rc;

pub struct Function {
    pub name: String,
    pub chunk: Chunk,
    pub signature: (Vec<Type>, Type),
}

impl Function {
    pub fn finish(self) -> Function {
        let Function { mut chunk, .. } = self;

        chunk.parent = None;

        Function { chunk, ..self }
    }
}

// script is sort of pseudo-function
pub struct Script<T: FromStack> {
    pub chunk: Chunk,
    phantom: PhantomData<T>,
}

impl<T: FromStack> Script<T> {
    pub fn new(chunk: Chunk) -> Script<T> {
        Script {
            chunk,
            phantom: PhantomData,
        }
    }
}

pub struct Frame<T: FromStack> {
    function: Either<Rc<Function>, Script<T>>,
    ip: usize,
    stack: usize,
}

impl<T: FromStack> Frame<T> {
    pub fn ip(&self) -> usize {
        self.ip
    }

    pub fn advance_ip(&mut self, amount: usize) {
        self.ip += amount;
    }

    pub fn retreat_ip(&mut self, amount: usize) {
        self.ip -= amount;
    }

    pub fn op_code(&self) -> OpCode {
        let op_byte = match &self.function {
            Either::Left(f) => f.chunk.code[self.ip],
            Either::Right(s) => s.chunk.code[self.ip],
        };

        OpCode::try_from(op_byte).expect("invalid instruction!!")
    }

    pub fn bytes(&self, range: Range<usize>) -> &[u8] {
        match &self.function {
            Either::Left(f) => &f.chunk.code[range],
            Either::Right(s) => &s.chunk.code[range],
        }
    }

    pub fn next_bytes(&mut self, len: usize) -> &[u8] {
        let value_slice = &self.bytes(self.ip..self.ip + len);
        self.ip += len;
        value_slice
    }
}
