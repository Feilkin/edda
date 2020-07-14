//! Bytecode definitions

use derive_try_from_primitive::TryFromPrimitive;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Error, Formatter};

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    /// Return from current function
    Return = 0x00,
    /// Return from block expression.
    /// First, pops A (u16) bytes from the stack as the return value,
    /// and after that, pops (discards) A + B (u16) bytes from the stack,
    /// and lastly, pushes return value back to stack.
    BlockReturn = 0x01,
    /// Push constant i32 to stack
    ConstantI32 = 0x10,
    /// Sum two i32s
    AddI32 = 0x30,
    /// Subtract two i32's
    SubI32 = 0x31,
    /// Multiple first i32 by second i32 from stack
    MulI32 = 0x32,
    /// Divide first i32 by second i32 from stack
    DivI32 = 0x33,
    /// Pop B, A from stack and push bool A < B to stack
    LessI32 = 0x34,
    /// Pop B, A from stack and push bool A <= B to stack
    LessEqualI32 = 0x35,
    /// Pop B, A from stack and push bool A == B to stack
    EqualI32 = 0x36,
    /// Unconditional relative jump to 16bit offset
    Jump = 0x50,
    /// Unconditional relative jump backwards
    Jumb = 0x51,
    /// Conditional relative jump to 16bit offset
    JumpIfFalse = 0x52,
    /// Pops N (u16) bytes from stack,
    PopN = 0x60,
    // reserved: long pop
    /// Pushes A (u16) bytes from stack offset B (u16) to top of stack
    GetLocal = 0x62,
    // reserved: long get
    /// Sets A (u16) bytes at stack offset B (u16) to value at top of stack
    SetLocal = 0x63,
    // reserved: long set
}

pub struct Chunk {
    pub code: Vec<u8>,
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        writeln!(f, "[ Addr | Op                   ]")?;
        let mut ptr = 0;

        while ptr < self.code.len() {
            write!(f, "[ {:04} | ", ptr)?;

            let op = OpCode::try_from(self.code[ptr]).unwrap();
            ptr += 1;

            write!(f, "{:<20}", format!("{:?}", op))?;

            match op {
                OpCode::Return
                | OpCode::AddI32
                | OpCode::SubI32
                | OpCode::MulI32
                | OpCode::DivI32
                | OpCode::LessEqualI32
                | OpCode::LessI32
                | OpCode::EqualI32 => (),
                OpCode::ConstantI32 => {
                    let arg = i32::from_le_bytes(self.code[ptr..ptr + 4].try_into().unwrap());
                    ptr += 4;

                    writeln!(f, " ]")?;
                    write!(f, "[      : {:>20}", arg)?;
                }
                OpCode::GetLocal | OpCode::SetLocal | OpCode::BlockReturn => {
                    let arg = u16::from_le_bytes(self.code[ptr..ptr + 2].try_into().unwrap());
                    ptr += 2;

                    writeln!(f, " ]")?;
                    write!(f, "[      : {:>20}", arg)?;
                    let arg = u16::from_le_bytes(self.code[ptr..ptr + 2].try_into().unwrap());
                    ptr += 2;

                    writeln!(f, " ]")?;
                    write!(f, "[      : {:>20}", arg)?;
                }
                OpCode::JumpIfFalse | OpCode::Jump | OpCode::Jumb | OpCode::PopN => {
                    let arg = u16::from_le_bytes(self.code[ptr..ptr + 2].try_into().unwrap());
                    ptr += 2;

                    writeln!(f, " ]")?;
                    write!(f, "[      : {:>20}", arg)?;
                }
            }

            writeln!(f, " ]")?;
        }

        Ok(())
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { code: Vec::new() }
    }

    pub fn push_op(&mut self, op: OpCode) {
        self.code.push(op as u8);
    }

    pub fn push_value<T: AsRef<[u8]>>(&mut self, value: T) {
        self.code.extend_from_slice(value.as_ref());
    }

    pub fn set_value<T: AsRef<[u8]>>(&mut self, value: T, offset: usize) {
        self.code[offset..offset + value.as_ref().len()].copy_from_slice(value.as_ref());
    }

    /// Returns next offset
    pub fn head(&self) -> usize {
        self.code.len()
    }

    /// Returns next offset as u16
    pub fn head_short(&self) -> u16 {
        self.head() as u16
    }
}
