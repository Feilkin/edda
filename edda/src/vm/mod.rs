//! Standard VM

use crate::vm::bytecode::{Chunk, OpCode};
use crate::vm::errors::OutOfMemory;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Error, Formatter};

pub mod bytecode;
pub mod compiler;
pub mod errors;

const SCRIPT_MEMORY: usize = 256;

// quick hack lol
pub enum VmState {
    Running(Vm),
    Finished(i32),
}

pub struct Vm {
    chunk: Chunk,
    mem: [u8; SCRIPT_MEMORY],
    /// Points to next empty memory location in stack
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

    pub fn run(mut self) -> VmState {
        let op = self.next_op();

        match op {
            OpCode::Return => {
                let ret = i32::from_le_bytes(self.pop_bytes(4).unwrap().try_into().unwrap());
                return VmState::Finished(ret);
            }
            OpCode::ConstantI32 => {
                self.load_bytes(4);
            }
            OpCode::AddI32 => {
                let (rhs, lhs) = self.pop_i32_2().unwrap();
                let val = lhs + rhs;
                self.push_bytes(&val.to_le_bytes()).unwrap();
            }
            OpCode::SubI32 => {
                let (rhs, lhs) = self.pop_i32_2().unwrap();
                let val = lhs - rhs;
                self.push_bytes(&val.to_le_bytes()).unwrap();
            }
            OpCode::MulI32 => {
                let (rhs, lhs) = self.pop_i32_2().unwrap();
                let val = lhs * rhs;
                self.push_bytes(&val.to_le_bytes()).unwrap();
            }
            OpCode::DivI32 => {
                let (rhs, lhs) = self.pop_i32_2().unwrap();
                let val = lhs / rhs;
                self.push_bytes(&val.to_le_bytes()).unwrap();
            }
            _ => unimplemented!(),
        }

        VmState::Running(self)
    }
}

impl Debug for Vm {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let op = OpCode::try_from(self.chunk.code[self.ip].try_into().unwrap()).unwrap();

        // TODO: Stack printing is only possible when exact type of all variables inside stack are
        //       known. For now, we only support i32 so it shouldn't be a problem :) gl future me

        let mut stack = Vec::new();

        for i in (self.sp + 1..SCRIPT_MEMORY - 1).step_by(4) {
            let val = i32::from_le_bytes(self.mem[i..i + 4].try_into().unwrap());
            stack.push(val);
        }

        let stack_repr = stack
            .iter()
            .rev()
            .fold(String::new(), |a, b| format!("{} {}", a, b));

        write!(
            f,
            "Vm {{ ip: {}, sp: {}, current op: {:?}, stack: [{} ] }}",
            self.ip, self.sp, op, stack_repr
        )
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
        let value_slice = &self.chunk.code[self.ip..self.ip + len];
        self.ip += len;
        value_slice
    }

    fn push_bytes(&mut self, bytes: &[u8]) -> Result<(), OutOfMemory> {
        let len = bytes.len();
        let sp = self.sp;

        // TODO: check that we don't run into heap??
        if sp + 1 < len {
            return Err(OutOfMemory {
                tried_to_allocate: len,
            });
        }

        let mut destination = &mut self.mem[sp - len + 1..sp + 1];
        destination.copy_from_slice(bytes);
        self.sp = sp - len;

        Ok(())
    }

    fn pop_bytes(&mut self, len: usize) -> Result<&[u8], OutOfMemory> {
        if self.sp + len >= SCRIPT_MEMORY {
            return Err(OutOfMemory {
                tried_to_allocate: len,
            });
        }

        let bytes = &self.mem[self.sp + 1..self.sp + 1 + len];
        self.sp += len;

        Ok(bytes)
    }

    fn pop_i32_2(&mut self) -> Result<(i32, i32), OutOfMemory> {
        let a = i32::from_le_bytes(self.pop_bytes(4)?.try_into().unwrap());
        let b = i32::from_le_bytes(self.pop_bytes(4)?.try_into().unwrap());

        Ok((a, b))
    }

    fn load_bytes(&mut self, len: usize) -> Result<(), OutOfMemory> {
        let sp = self.sp;

        // TODO: check that we don't run into heap??
        if sp + 1 < len {
            return Err(OutOfMemory {
                tried_to_allocate: len,
            });
        }

        let src = &self.chunk.code[self.ip..self.ip + len];
        self.ip += len;

        let mut destination = &mut self.mem[sp - len + 1..sp + 1];
        destination.copy_from_slice(src);
        self.sp = sp - len;

        Ok(())
    }
}
