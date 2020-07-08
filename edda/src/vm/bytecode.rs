//! Bytecode definitions

use derive_try_from_primitive::TryFromPrimitive;

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    /// Return from current function
    Return,
    /// Push constant i32 to stack
    ConstantI32,
    /// Sum two i32s
    AddI32,
}

pub struct Chunk {
    pub code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
        }
    }

    pub fn push_op(&mut self, op: OpCode) {
        self.code.push(op as u8);
    }

    pub fn push_value<T: AsRef<[u8]>>(&mut self, value: T) {
        self.code.extend_from_slice(value.as_ref());
    }
}