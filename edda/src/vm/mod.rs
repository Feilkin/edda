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

type OpFn = fn(&mut Vm) -> Option<i32>;

fn undefined_op(_: &mut Vm) -> Option<i32> {
    panic!("undefined opcode");
}

lazy_static! {
    static ref OP_FN_LOOKUP: [fn(&mut Vm) -> Option<i32>; 256] = {
        let mut lookup_table = [undefined_op as OpFn; 256];
        let lookup_values: Vec<OpFn> = (0..=255)
            .map(|b| {
                OpCode::try_from(b)
                    .and_then(|op| Some(Vm::op_fn(op)))
                    .unwrap_or(undefined_op)
            })
            .collect();

        lookup_table.copy_from_slice(&lookup_values[..]);

        lookup_table
    };
}

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

    pub fn run(mut self) -> i32 {
        loop {
            let op = self.next_op();
            if let Some(ret) = OP_FN_LOOKUP[op as u8 as usize](&mut self) {
                return ret;
            }
        }
    }

    fn op_fn(op: OpCode) -> fn(&mut Vm) -> Option<i32> {
        match op {
            OpCode::Return => |vm| {
                let ret = i32::from_le_bytes(vm.pop_bytes(4).unwrap().try_into().unwrap());
                Some(ret)
            },
            OpCode::ConstantI32 => |vm| {
                vm.load_bytes(4).unwrap();
                None
            },
            OpCode::AddI32 => |vm| {
                let (rhs, lhs) = vm.pop_i32_2().unwrap();
                let val = lhs + rhs;
                vm.push_bytes(&val.to_le_bytes()).unwrap();
                None
            },
            OpCode::SubI32 => |vm| {
                let (rhs, lhs) = vm.pop_i32_2().unwrap();
                let val = lhs - rhs;
                vm.push_bytes(&val.to_le_bytes()).unwrap();
                None
            },
            OpCode::MulI32 => |vm| {
                let (rhs, lhs) = vm.pop_i32_2().unwrap();
                let val = lhs * rhs;
                vm.push_bytes(&val.to_le_bytes()).unwrap();
                None
            },
            OpCode::DivI32 => |vm| {
                let (rhs, lhs) = vm.pop_i32_2().unwrap();
                let val = lhs / rhs;
                vm.push_bytes(&val.to_le_bytes()).unwrap();
                None
            },
            OpCode::Jump => |vm| {
                let offset = vm.chunk_short();
                vm.ip += offset as usize - 2;
                None
            },
            OpCode::Jumb => |vm| {
                let offset = vm.chunk_short();
                vm.ip -= offset as usize + 2;
                None
            },
            OpCode::JumpIfFalse => |vm| {
                let offset = vm.chunk_short();
                let cond = vm.pop_bytes(1).unwrap();

                if cond[0] == 0 {
                    vm.ip += offset as usize - 2;
                }
                None
            },
            OpCode::LessI32 => |vm| {
                let (rhs, lhs) = vm.pop_i32_2().unwrap();
                let val = lhs < rhs;
                vm.push_bytes(&[val as u8]).unwrap();
                None
            },
            OpCode::LessEqualI32 => |vm| {
                let (rhs, lhs) = vm.pop_i32_2().unwrap();
                let val = lhs <= rhs;
                vm.push_bytes(&[val as u8]).unwrap();
                None
            },
            OpCode::EqualI32 => |vm| {
                let (rhs, lhs) = vm.pop_i32_2().unwrap();
                let val = lhs == rhs;
                vm.push_bytes(&[val as u8]).unwrap();
                None
            },
            OpCode::PopN => |vm| {
                let size = vm.chunk_short() as usize;
                vm.pop_bytes(size).unwrap();
                None
            },
            OpCode::GetLocal => |vm| {
                let size = vm.chunk_short() as usize;
                let offset = vm.chunk_short() as usize;

                let top = vm
                    .call_stack
                    .last()
                    .map(|f| f.stack_start)
                    .unwrap_or(SCRIPT_MEMORY);

                vm.push_slice_from_mem(top - offset - size..top - offset)
                    .unwrap();
                None
            },
            OpCode::LoadFunction => |vm| {
                vm.load_bytes(2).unwrap();
                None
            },
            OpCode::BlockReturn => |vm| {
                let ret_size = vm.chunk_short() as usize;
                let scope_size = vm.chunk_short() as usize;

                // Instead of allocating temp buffer for the return value, we can just use a slice
                // on vm.mem, as this memory will not get mutated.
                let ret_range = vm.sp + 1..vm.sp + ret_size + 1;
                vm.pop_bytes(scope_size + ret_size).unwrap();
                vm.push_slice_from_mem(ret_range).unwrap();
                None
            },
            OpCode::Call => |vm| {
                let stack_size = vm.chunk_short() as usize;
                let return_size = vm.chunk_short() as usize;
                let return_address = vm.ip;
                let fn_address = u16::from_le_bytes(vm.pop_bytes(2).unwrap().try_into().unwrap());

                vm.call_stack.push(CallFrame {
                    stack_start: vm.sp + 1 + stack_size,
                    return_address,
                    stack_size,
                    return_size,
                });

                vm.ip = fn_address as usize;
                None
            },
            OpCode::FnReturn => |vm| {
                if vm.call_stack.is_empty() {
                    panic!("tried to pop call frame from empty call stack");
                }

                let CallFrame {
                    return_address,
                    stack_size,
                    return_size,
                    ..
                } = vm.call_stack.pop().unwrap();

                // Instead of allocating temp buffer for the return value, we can just use a slice
                // on vm.mem, as this memory will not get mutated.
                let ret_range = vm.sp + 1..vm.sp + return_size + 1;
                vm.pop_bytes(stack_size + return_size).unwrap();
                vm.push_slice_from_mem(ret_range).unwrap();
                vm.ip = return_address;
                None
            },
            u @ _ => unimplemented!("opcode {:?} is not implemented!", u),
        }
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
    fn next_op(&mut self) -> u8 {
        let op = self.chunk.code[self.ip];
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
