//! Standard VM

use crate::typer::{AsEddaType, FromStack};
use crate::vm::bytecode::{Chunk, OpCode};
use crate::vm::errors::OutOfMemory;
use crate::vm::function::{Frame, Script};
use itertools::Either;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Error, Formatter};
use std::ops::Range;

pub mod bytecode;
pub mod compiler;
pub mod errors;
mod function;

const SCRIPT_MEMORY: usize = 256;

pub enum VmState<T: AsEddaType + FromStack> {
    Running(Vm<T>),
    Finished(T),
}

pub struct Vm<T: AsEddaType + FromStack> {
    mem: [u8; SCRIPT_MEMORY],
    /// Points to next empty memory location in stack
    sp: usize,
    frames: Vec<Frame<T>>,
    current_frame: usize,
}

// public methods
impl<T: AsEddaType + FromStack> Vm<T> {
    pub fn new(script: Script<T>) -> Vm<T> {
        let sp = SCRIPT_MEMORY - 1;

        let root_frame = Frame {
            function: Either::Right(script),
            ip: 0,
            stack: sp,
        };

        Vm {
            frames: vec![root_frame],
            mem: [0; SCRIPT_MEMORY],
            sp,
            current_frame: 0,
        }
    }

    pub fn run(mut self) -> VmState<T> {
        let op = self.next_op();

        match op {
            OpCode::Return => {
                return VmState::Finished(T::pop(&mut self));
            }
            OpCode::ConstantI32 => {
                self.load_bytes(4);
            }
            OpCode::ConstantU16 => {
                self.load_bytes(2);
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
                self.advance_ip(offset as usize - 2);
            }
            OpCode::Jumb => {
                let offset = self.chunk_short();
                self.advance_ip(offset as usize + 2);
            }
            OpCode::JumpIfFalse => {
                let offset = self.chunk_short();
                let cond = self.pop_bytes(1).unwrap();

                if cond[0] == 0 {
                    self.advance_ip(offset as usize - 2);
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
                self.pop_bytes(size);
            }
            OpCode::GetLocal => {
                let size = self.chunk_short() as usize;
                let offset = self.chunk_short() as usize;

                self.push_slice_from_mem(SCRIPT_MEMORY - offset - size..SCRIPT_MEMORY - offset)
                    .unwrap();
            }
            OpCode::BlockReturn => {
                let ret_size = self.chunk_short() as usize;
                let scope_size = self.chunk_short() as usize;

                // Instead of allocating temp buffer for the return value, we can just use a slice
                // on self.mem, as this memory will not get mutated.
                let ret_range = self.sp + 1..self.sp + ret_size + 1;
                self.pop_bytes(scope_size + ret_size);
                self.push_slice_from_mem(ret_range).unwrap();
            }
            u @ _ => unimplemented!("opcode {:?} is not implemented!", u),
        }

        VmState::Running(self)
    }
}

//impl<T: AsEddaType + FromStack> Debug for Vm<T> {
//    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
//        let op = OpCode::try_from(self.chunk.code[self.ip].try_into().unwrap()).unwrap();
//
//        // TODO: Stack printing is only possible when exact type of all variables inside stack are
//        //       known. For now, we only support i32 so it shouldn't be a problem :) gl future me
//
//        //        let mut stack = Vec::new();
//        //
//        //        for i in (self.sp + 1..SCRIPT_MEMORY - 1).step_by(4) {
//        //            let val = i32::from_le_bytes(self.mem[i..i + 4].try_into().unwrap());
//        //            stack.push(val);
//        //        }
//        //
//        //        let stack_repr = stack
//        //            .iter()
//        //            .rev()
//        //            .fold(String::new(), |a, b| format!("{} {}", a, b));
//
//        write!(
//            f,
//            "Vm {{ ip: {}, sp: {}, current op: {:?} }}",
//            self.ip,
//            self.sp,
//            op, //stack_repr
//        )
//    }
//}

// private methods
impl<T: AsEddaType + FromStack> Vm<T> {
    fn frame(&self) -> &Frame<T> {
        &self.frames[self.current_frame]
    }

    fn frame_mut(&mut self) -> &mut Frame<T> {
        &mut self.frames[self.current_frame]
    }

    fn advance_ip(&mut self, amount: usize) {
        self.frame_mut().advance_ip(amount)
    }

    fn retreat_ip(&mut self, amount: usize) {
        self.frame_mut().retreat_ip(amount)
    }

    fn ip(&self) -> usize {
        self.frame().ip()
    }

    fn next_op(&mut self) -> OpCode {
        let op = self.frame().op_code();
        self.advance_ip(1);
        op
    }

    fn next_bytes(&mut self, len: usize) -> &[u8] {
        self.frame_mut().next_bytes(len)
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

    pub fn pop_bytes(&mut self, len: usize) -> Result<&[u8], OutOfMemory> {
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
        if sp + 1 < len {
            return Err(OutOfMemory {
                tried_to_allocate: len,
            });
        }

        let src = self.next_bytes(len);

        let mut destination = &mut self.mem[sp - len + 1..sp + 1];
        destination.copy_from_slice(src);
        self.sp = sp - len;

        Ok(())
    }

    fn chunk_short(&mut self) -> u16 {
        u16::from_le_bytes(self.next_bytes(2).try_into().unwrap())
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

        let (src, dst) = if sp < range.start {
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
