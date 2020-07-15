//! Standard VM

use crate::vm::bytecode::{Chunk, OpCode};
use crate::vm::errors::OutOfMemory;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Error, Formatter};
use std::ops::Range;

pub mod bytecode;
pub mod compiler;
pub mod errors;
mod function;

const SCRIPT_MEMORY: usize = 1024;

// quick hack lol
pub enum VmState {
    Running(Vm),
    Finished(i32),
}

pub struct Vm {
    chunk: Chunk,
    mem: [u8; SCRIPT_MEMORY],
    call_stack: Vec<CallFrame>,
    /// Points to next empty memory location in stack
    sp: usize,
    ip: usize,
}

pub struct CallFrame {
    return_address: usize,
    stack_start: usize,
    stack_size: usize,
    return_size: usize,
}

// public methods
impl Vm {
    pub fn new(chunk: Chunk) -> Vm {
        Vm {
            chunk,
            mem: [0; SCRIPT_MEMORY],
            call_stack: Vec::new(),
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
                self.load_bytes(4).unwrap();
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
            OpCode::Jump => {
                let offset = self.chunk_short();
                self.ip += offset as usize - 2;
            }
            OpCode::Jumb => {
                let offset = self.chunk_short();
                self.ip -= offset as usize + 2;
            }
            OpCode::JumpIfFalse => {
                let offset = self.chunk_short();
                let cond = self.pop_bytes(1).unwrap();

                if cond[0] == 0 {
                    self.ip += offset as usize - 2;
                }
            }
            OpCode::LessI32 => {
                let (rhs, lhs) = self.pop_i32_2().unwrap();
                let val = lhs < rhs;
                self.push_bytes(&[val as u8]).unwrap();
            }
            OpCode::LessEqualI32 => {
                let (rhs, lhs) = self.pop_i32_2().unwrap();
                let val = lhs <= rhs;
                self.push_bytes(&[val as u8]).unwrap();
            }
            OpCode::EqualI32 => {
                let (rhs, lhs) = self.pop_i32_2().unwrap();
                let val = lhs == rhs;
                self.push_bytes(&[val as u8]).unwrap();
            }
            OpCode::PopN => {
                let size = self.chunk_short() as usize;
                self.pop_bytes(size).unwrap();
            }
            OpCode::GetLocal => {
                let size = self.chunk_short() as usize;
                let offset = self.chunk_short() as usize;

                let top = self
                    .call_stack
                    .last()
                    .map(|f| f.stack_start)
                    .unwrap_or(SCRIPT_MEMORY);

                self.push_slice_from_mem(top - offset - size..top - offset)
                    .unwrap();
            }
            OpCode::LoadFunction => {
                self.load_bytes(2).unwrap();
            }
            OpCode::BlockReturn => {
                let ret_size = self.chunk_short() as usize;
                let scope_size = self.chunk_short() as usize;

                // Instead of allocating temp buffer for the return value, we can just use a slice
                // on self.mem, as this memory will not get mutated.
                let ret_range = self.sp + 1..self.sp + ret_size + 1;
                self.pop_bytes(scope_size + ret_size).unwrap();
                self.push_slice_from_mem(ret_range).unwrap();
            }
            OpCode::Call => {
                let stack_size = self.chunk_short() as usize;
                let return_size = self.chunk_short() as usize;
                let return_address = self.ip;
                let fn_address = u16::from_le_bytes(self.pop_bytes(2).unwrap().try_into().unwrap());

                self.call_stack.push(CallFrame {
                    stack_start: self.sp + 1 + stack_size,
                    return_address,
                    stack_size,
                    return_size,
                });

                self.ip = fn_address as usize;
            }
            OpCode::FnReturn => {
                if self.call_stack.is_empty() {
                    panic!("tried to pop call frame from empty call stack");
                }

                let CallFrame {
                    return_address,
                    stack_size,
                    return_size,
                    ..
                } = self.call_stack.pop().unwrap();

                // Instead of allocating temp buffer for the return value, we can just use a slice
                // on self.mem, as this memory will not get mutated.
                let ret_range = self.sp + 1..self.sp + return_size + 1;
                self.pop_bytes(stack_size + return_size).unwrap();
                self.push_slice_from_mem(ret_range).unwrap();
                self.ip = return_address;
            }
            u @ _ => unimplemented!("opcode {:?} is not implemented!", u),
        }

        VmState::Running(self)
    }
}

impl Debug for Vm {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let op = OpCode::try_from(self.chunk.code[self.ip].try_into().unwrap()).unwrap();

        // TODO: Stack printing is only possible when exact type of all variables inside stack are
        //       known. For now, we only support i32 so it shouldn't be a problem :) gl future me

        //        let mut stack = Vec::new();
        //
        //        for i in (self.sp + 1..SCRIPT_MEMORY - 1).step_by(4) {
        //            let val = i32::from_le_bytes(self.mem[i..i + 4].try_into().unwrap());
        //            stack.push(val);
        //        }
        //
        //        let stack_repr = stack
        //            .iter()
        //            .rev()
        //            .fold(String::new(), |a, b| format!("{} {}", a, b));

        write!(
            f,
            "Vm {{ ip: {}, sp: {}, current op: {:?} }}",
            self.ip,
            self.sp,
            op, //stack_repr
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

    /// Pushed bytes to top of stack
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

    fn pop_u8(&mut self) -> Result<u8, OutOfMemory> {
        let val = u8::from_le_bytes(self.pop_bytes(1)?.try_into().unwrap());
        Ok(val)
    }

    fn load_bytes(&mut self, len: usize) -> Result<(), OutOfMemory> {
        let sp = self.sp;

        // TODO: check that we don't run into heap??
        if sp < len {
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

    fn chunk_short(&mut self) -> u16 {
        let val = u16::from_le_bytes(self.chunk.code[self.ip..self.ip + 2].try_into().unwrap());
        self.ip += 2;
        val
    }

    /// Pushes bytes indicated by `range` to top of stack
    fn push_slice_from_mem(&mut self, range: Range<usize>) -> Result<(), OutOfMemory> {
        let sp = self.sp;
        let len = range.end - range.start;

        // TODO: check that we don't run into heap??
        if sp + 1 < len {
            return Err(OutOfMemory {
                tried_to_allocate: len,
            });
        }

        let (src, dst) = if sp <= range.start {
            // value is contained within stack (local variable)
            // stack grows from the top down, so dst and src are reversed here
            let (dst, src) = self.mem.split_at_mut(range.start);
            let src = &src[0..len];
            let dst = &mut dst[sp - len + 1..sp + 1];

            (src, dst)
        } else {
            // value is below stack (heap allocated, or return value)
            let (src, dst) = self.mem.split_at_mut(sp - len + 1);
            let src = &src[range.start..range.end];
            let dst = &mut dst[0..len];

            (src, dst)
        };

        dst.copy_from_slice(src);

        self.sp = sp - len;

        Ok(())
    }
}
