//! Bytecode stuff
use std::convert::TryFrom;

#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    Return = 0x01,
    ConstantU8 = 0x02,
    ConstantF64 = 0x04,
    NegateF64 = 0x32,
}

// TODO: find a good way to automate this
impl TryFrom<u8> for OpCode {
    type Error = &'static str;

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        match byte {
            0x01 => Ok(OpCode::Return),
            0x02 => Ok(OpCode::ConstantU8),
            0x04 => Ok(OpCode::ConstantF64),
            0x32 => Ok(OpCode::NegateF64),
            _ => Err("Invalid OpCode!"),
        }
    }
}

// TODO: move to Constant crate?
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

type ConstantError = &'static str;
type ConstantPool = Vec<u8>;

pub trait Constant: Sized {
    fn push(self, pool: &mut ConstantPool) -> usize;
    fn get(index: usize, pool: &ConstantPool) -> Result<(Self, usize), ConstantError>;
}

// TODO: move to different file??
impl Constant for u8 {
    fn push(self, pool: &mut ConstantPool) -> usize {
        pool.push(self);

        1
    }

    fn get(index: usize, pool: &ConstantPool) -> Result<(u8, usize), ConstantError> {
        // TODO: error handling
        Ok((pool[index], 1))
    }
}

impl Constant for f64 {
    fn push(self, pool: &mut ConstantPool) -> usize {
        let mut buf = [0u8; 8];
        buf.as_mut().write_f64::<LittleEndian>(self).unwrap();

        pool.extend(&buf);
        8
    }

    fn get(index: usize, pool: &ConstantPool) -> Result<(f64, usize), ConstantError> {
        // TODO: error handling
        let double = (&pool[index .. index + 8]).read_f64::<LittleEndian>().unwrap();

        Ok((double, 8))
    }
}

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<u8>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn lines(&self) -> &[usize] {
        &self.lines
    }

    pub fn constants(&self) -> &[u8] {
        &self.constants
    }

    pub fn push_byte<T: Into<u8>>(&mut self, byte: T, line: usize) {
        self.code.push(byte.into());
        self.lines.push(line);
    }

    pub fn push_bytes(&mut self, bytes: &[u8], line: usize) {
        self.code.extend(bytes);
        for i in 1 .. bytes.len() {
            self.lines.push(line)
        }
    }

    pub fn push_op(&mut self, op: OpCode, line: usize) {
        self.push_byte(op as u8, line);
    }

    pub fn add_constant<T: Constant>(&mut self, constant: T) -> usize {
        let index = self.constants.len();
        T::push(constant, &mut self.constants);

        index
    }

    pub fn get_constant<T: Constant>(&self, index: usize) -> T {
        let (constant, _size) = T::get(index, &self.constants).unwrap();
        
        constant
    }
}

#[test]
fn test_chunk() {
    let mut chunk = Chunk::new();
    let constant_index = chunk.add_constant(123);
    chunk.push_op(OpCode::ConstantU8, 1);
    chunk.push_byte(constant_index as u8, 1);
    chunk.push_op(OpCode::Return, 1);

    use super::debug::disassemble_chunk;
    disassemble_chunk(&chunk, "test_chunk");
}

#[test]
fn test_double() {
    let mut chunk = Chunk::new();
    let constant_index = chunk.add_constant(133.7);
    chunk.push_op(OpCode::ConstantF64, 1);
    chunk.push_byte(constant_index as u8, 1);
    chunk.push_op(OpCode::Return, 1);

    use super::debug::disassemble_chunk;
    disassemble_chunk(&chunk, "test_double");
}