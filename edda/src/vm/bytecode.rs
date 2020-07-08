//! Bytecode definitions

use derive_try_from_primitive::TryFromPrimitive;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Error, Formatter};

#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    /// Return from current function
    Return = 0x00,
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
                | OpCode::DivI32 => (),
                OpCode::ConstantI32 => {
                    let arg = i32::from_le_bytes(self.code[ptr..ptr + 4].try_into().unwrap());
                    ptr += 4;

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
}
