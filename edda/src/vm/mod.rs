//! Standard VM

use crate::vm::bytecode::{Chunk, OpCode};
use std::convert::{TryFrom, TryInto};
use crate::vm::errors::OutOfMemory;

pub mod bytecode;
pub mod errors;
pub mod compiler;

const SCRIPT_MEMORY: usize = 256;

pub struct Vm {
    chunk: Chunk,
    mem: [u8; SCRIPT_MEMORY],
    sp: usize,
    ip: usize,
}

// public methods
impl Vm {
    pub fn new(chunk: Chunk) -> Vm {
        Vm {
            chunk,
            mem: [0; SCRIPT_MEMORY],
            sp: SCRIPT_MEMORY - 1,
            ip: 0,
        }
    }

    pub fn run(mut self) -> Vm {
        let op = self.next_op();
        self.ip += 1;

        match op {
            OpCode::ConstantI32 => {
                self.load_bytes(4);
            },
            OpCode::AddI32 => {
                let lhs = i32::from_le_bytes(self.pop_bytes(4).unwrap().try_into().unwrap());
                let rhs = i32::from_le_bytes(self.pop_bytes(4).unwrap().try_into().unwrap());
                let val = lhs + rhs;
                self.push_bytes(&val.to_le_bytes()).unwrap();
            }
            _ => unimplemented!()
        }

        self
    }
}

// private methods
impl Vm {
    fn next_op(&mut self) -> OpCode {
        let op = OpCode::try_from(self.chunk.code[self.ip]).unwrap();
        self.ip += 1;
        op
    }

    fn next_bytes(&mut self, len: usize) -> &[u8] {
        let value_slice = &self.chunk.code[self.ip .. self.ip + len];
        self.ip += len;
        value_slice
    }

    fn push_bytes(&mut self, bytes: &[u8]) -> Result<(), OutOfMemory>{
        let len = bytes.len();
        let sp = self.sp;

        // TODO: check that we don't run into heap??
        if sp + 1 < len {
            return Err(OutOfMemory {
                tried_to_allocate: len,
            });
        }

        let mut destination = &mut self.mem[sp - len .. sp];
        destination.copy_from_slice(bytes);
        self.sp = sp - len - 1;

        Ok(())
    }

    fn pop_bytes(&mut self, len: usize) -> Result<&[u8], OutOfMemory> {
        if self.sp + len >= SCRIPT_MEMORY {
            return Err(OutOfMemory {
                tried_to_allocate: len,
            });
        }

        let bytes = &self.mem[self.sp .. self.sp + len];
        self.sp += len;

        Ok(bytes)
    }

    fn load_bytes(&mut self, len: usize) -> Result<(), OutOfMemory> {
        let sp = self.sp;

        // TODO: check that we don't run into heap??
        if sp + 1 < len {
            return Err(OutOfMemory {
                tried_to_allocate: len,
            });
        }

        let src = &self.chunk.code[self.ip .. self.ip + len];
        self.ip += len;

        let mut destination = &mut self.mem[sp - len .. sp];
        destination.copy_from_slice(src);
        self.sp = sp - len - 1;

        Ok(())
    }
}