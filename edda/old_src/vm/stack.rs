//!
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

const MAX_STACK: usize = 512;

pub struct Stack {
    mem: [u8; MAX_STACK],
    /// Points past the last item
    top: usize,
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            mem: [0; MAX_STACK],
            top: 0,
        }
    }

    pub fn push<'a, T: Value<'a>>(&mut self, value: T) {
        Value::push(value, self);
    }

    pub fn pop<'a, T: Value<'a>>(&mut self) -> T {
        Value::pop(self)
    }

    pub fn push_bytes(&mut self, bytes: &[u8]) {
        let len = bytes.len();

        for i in 0 .. len {
            self.mem[self.top + i] = bytes[i];
        }

        self.top += len;
    }

    pub fn pop_bytes(&mut self, len: usize) -> &[u8] {
        let bytes = &self.mem[self.top - len .. self.top];
        self.top -= len;

        &bytes
    }
}

use super::chunk::Constant;

pub trait Value<'a> : Constant<'a> {
    fn push(self, stack: &mut Stack) -> ();
    fn pop(stack: &mut Stack) -> Self;
}

impl<'a> Value<'a> for f64 {
    fn push(self, stack: &mut Stack) {
        let mut buf = [0u8; 8];
        buf.as_mut().write_f64::<LittleEndian>(self).unwrap();
        stack.push_bytes(&buf);
    }

    fn pop(stack: &mut Stack) -> f64 {
        let mut byte_slice = stack.pop_bytes(8);
        let double = byte_slice.read_f64::<LittleEndian>().unwrap();

        double
    }
}

impl<'a> Value<'a> for () {
    fn push(self, _stack: &mut Stack) {}

    fn pop(_stack: &mut Stack) -> () { () }
}