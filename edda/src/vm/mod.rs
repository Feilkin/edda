mod chunk;
mod debug;
mod stack;
mod compiler;

use chunk::Chunk;
use stack::Stack;

pub use compiler::Compiler;
pub use stack::Value;

pub type InterpretResult<'a, T: Value<'a>> = Result<T, InterpretError>;

#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError,
}

pub struct Vm {
    enable_tracing: bool,
    stack: Stack,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            enable_tracing: false,
            stack: Stack::new(),
        }
    }

    pub fn enable_tracing(&mut self) {
        self.enable_tracing = true;
    }

    pub fn interpret<'a, T: Value<'a>>(&mut self, chunk: &Chunk) -> InterpretResult<T> {
        use std::convert::TryInto;
        use chunk::OpCode;

        let mut ip = 0;

        loop {
            let op = chunk.code()[ip].try_into().unwrap(); // TODO: error handling

            if self.enable_tracing {
                debug::disassemble_instruction(&chunk, ip);
            }

            // TODO: use macro or something for constants
            match op {
                OpCode::Return => {
                    let val = self.stack.pop::<T>();

                    return Ok(val)
                },
                OpCode::ConstantU8 => {
                    ip += 1;
                    let constant_index = chunk.code()[ip].try_into().unwrap();
                    let constant = chunk.get_constant::<u8>(constant_index);
                    self.stack.push(constant as f64);
                },
                OpCode::ConstantF64 => {
                    ip += 1;
                    let constant_index = chunk.code()[ip].try_into().unwrap();
                    let constant = chunk.get_constant::<f64>(constant_index);
                    self.stack.push(constant)
                },
                OpCode::AddF64 => {
                    let b = self.stack.pop::<f64>();
                    let a = self.stack.pop::<f64>();
                    self.stack.push::<f64>(a + b);
                },
                OpCode::SubstractF64 => {
                    let b = self.stack.pop::<f64>();
                    let a = self.stack.pop::<f64>();
                    self.stack.push::<f64>(a - b);
                },
                OpCode::MultiplyF64 => {
                    let b = self.stack.pop::<f64>();
                    let a = self.stack.pop::<f64>();
                    self.stack.push::<f64>(a * b);
                },
                OpCode::DivideF64 => {
                    let b = self.stack.pop::<f64>();
                    let a = self.stack.pop::<f64>();
                    self.stack.push::<f64>(a / b);
                },
                OpCode::NegateF64 => {
                    let val = self.stack.pop::<f64>();
                    self.stack.push::<f64>(-val);
                }
                _ => unimplemented!(),
            }

            ip += 1;
        }
    }
}

#[test]
fn test_vm() {
    use chunk::OpCode;

    let mut chunk = Chunk::new();
    let a = chunk.add_constant(133.7);
    let b = chunk.add_constant(666f64);
    chunk.push_op(OpCode::ConstantF64, 1);
    chunk.push_byte(a as u8, 1);

    chunk.push_op(OpCode::ConstantF64, 2);
    chunk.push_byte(b as u8, 2);

    chunk.push_op(OpCode::AddF64, 3);

    chunk.push_op(OpCode::NegateF64, 1);
    chunk.push_op(OpCode::Return, 1);

    //use debug::disassemble_chunk;
    //disassemble_chunk(&chunk, "test_vm");

    let mut vm = Vm::new();
    vm.enable_tracing();
    
    let val = vm.interpret::<f64>(&chunk).unwrap();

    println!("val: {}", val);
}