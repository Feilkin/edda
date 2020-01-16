//! VM debugging stuff

use super::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.code().len() {
        offset = disassemble_instruction(chunk, offset).unwrap();
    }
}

pub fn disassemble_instruction(chunk: &Chunk, mut offset: usize) -> Result<usize, &'static str> {
    use std::convert::TryInto;

    let op = chunk.code()[offset];
    let line = chunk.lines()[offset];

    print!("{:04} | ", offset);
    
    if offset > 0 && line == chunk.lines()[offset - 1] {
        print!("   | ");
    } else {
        print!("{:04} ", line);
    }
    
    offset += 1;

    // TODO: use macro or something for DRYness
    match op.try_into()? {
        OpCode::ConstantU8 =>  {
            let constant_index = chunk.code()[offset];
            offset += 1;
            let constant_val = chunk.get_constant::<u8>(constant_index as usize);
            print!("{:?}\t {}\t '{}'", OpCode::ConstantU8, constant_index, constant_val);
        },
        OpCode::ConstantF64 =>  {
            let constant_index = chunk.code()[offset];
            offset += 1;
            let constant_val = chunk.get_constant::<f64>(constant_index as usize);
            print!("{:?}\t {}\t '{}'", OpCode::ConstantF64, constant_index, constant_val);
        },
        op @ OpCode::NegateF64 |
        op @ OpCode::Return |
        op @ OpCode::AddF64 |
        op @ OpCode::SubstractF64 |
        op @ OpCode::MultiplyF64 |
        op @ OpCode::DivideF64 => print!("{:?}", op),
    };

    println!("");

    return Ok(offset);
}