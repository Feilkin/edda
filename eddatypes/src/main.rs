//! Some testing with types and encoding and stuff

use std::fmt::Debug;
use std::mem::size_of;
use std::str;

// some boring stuff at the start, see usage in `fn main()` at the bottom

#[derive(Debug)]
struct Stack(Vec<u8>);

impl Stack {
    fn new() -> Stack {
        Stack(Vec::new())
    }

    fn push<T>(&mut self, item: T) -> ()
    where
        T: StackPush + Debug,
    {
        println!("Pushing {:?}", item);
        item.stack_push(&mut self.0);
    }

    fn pop<T>(&mut self) -> T
    where
        T: StackPop + Debug,
    {
        let item = T::stack_pop(&mut self.0);
        println!("Popping {:?}", item);
        item
    }

    fn get_args<A, R, F: Fn(A) -> R>(&mut self, _f: &F) -> A
    where
        A: StackPop,
        R: StackPush,
    {
        A::stack_pop(&mut self.0)
    }

    fn call<A, R, F: Fn(A) -> R>(&mut self, f: F)
    where
        A: StackPop,
        R: StackPush,
    {
        f(self.get_args(&f)).stack_push(&mut self.0);
    }
}

trait StackPush {
    fn stack_push(self, stack: &mut Vec<u8>);
}

trait StackPop {
    fn stack_pop(stack: &mut Vec<u8>) -> Self;
}

impl StackPush for u8 {
    fn stack_push(self, stack: &mut Vec<u8>) {
        stack.push(self);
    }
}

impl StackPop for u8 {
    fn stack_pop(stack: &mut Vec<u8>) -> u8 {
        stack.pop().unwrap()
    }
}

impl StackPush for usize {
    fn stack_push(self, stack: &mut Vec<u8>) {
        let size = size_of::<usize>();
        for i in 0..size {
            stack.push((self >> (i * 8) & 0xFF) as u8);
        }
    }
}

impl StackPop for usize {
    fn stack_pop(stack: &mut Vec<u8>) -> usize {
        let size = size_of::<usize>();
        let mut val = 0;
        for i in 0..size {
            let b = u8::stack_pop(stack) as usize;
            val |= b << ((size - 1) - i) * 8;
        }

        val
    }
}

impl<'a> StackPush for &'a str {
    fn stack_push(self, stack: &mut Vec<u8>) {
        let len = self.len() as u8;
        stack.extend(self.bytes());
        stack.push(len);
    }
}

impl StackPop for String {
    fn stack_pop(stack: &mut Vec<u8>) -> String {
        let len = stack.pop().unwrap() as usize;

        if stack.len() < len {
            panic!("Stack underflow!");
        }

        let start = stack.len() - len;
        let end = stack.len();
        let string = str::from_utf8(&stack[start..end]).unwrap().to_owned();
        stack.truncate(start);

        string
    }
}

impl<T> StackPush for Vec<T>
where
    T: StackPush,
{
    fn stack_push(self, stack: &mut Vec<u8>) {
        let len = self.len();
        for item in self.into_iter().rev() {
            item.stack_push(stack);
        }

        len.stack_push(stack);
    }
}
impl<T> StackPop for Vec<T>
where
    T: StackPop,
{
    fn stack_pop(stack: &mut Vec<u8>) -> Vec<T> {
        let len = usize::stack_pop(stack);
        let mut v = Vec::new();

        for _ in 0..len {
            let val = T::stack_pop(stack);
            v.push(val);
        }

        v
    }
}

impl<A, B> StackPush for (A, B)
where
    A: StackPush,
    B: StackPush,
{
    fn stack_push(self, stack: &mut Vec<u8>) {
        self.1.stack_push(stack);
        self.0.stack_push(stack);
        stack.push(2);
    }
}

impl<A, B> StackPop for (A, B)
where
    A: StackPop,
    B: StackPop,
{
    fn stack_pop(stack: &mut Vec<u8>) -> (A, B) {
        let arity = stack.pop().unwrap();

        if arity != 2 {
            panic!("Wrong arity: {}, expected 2", arity);
        }

        let a = A::stack_pop(stack);
        let b = B::stack_pop(stack);

        (a, b)
    }
}

fn main() {
    // lets create new stack
    let mut stack = Stack::new();

    // push some basic stuff like strings, tuples, and vectors
    stack.push("Hello, world!");
    stack.push::<(((u8, u8), u8), u8)>((((1, 2), 3), 4));
    stack.push::<Vec<u8>>(vec![
        0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9,
    ]);

    // print the stack so we can marvel at its glory
    println!("stack: {:?}", stack);

    // ok, lets get our stuff back from the stack
    let v = stack.pop::<Vec<u8>>();
    println!(">> {:?}", v);

    // the type hints look a bit silly, probably wouldn't need them in real life use
    let t = stack.pop::<(((u8, u8), u8), u8)>();
    println!(">> {:?}", t);

    let s = stack.pop::<String>();
    println!(">> {:?}", s);

    // lets try getting function args from the stack
    // first, we push the arguments to the stack (as a tuple, because rust doesn't have variadic params)
    stack.push((1u8, 2u8));

    // now, call the function using this one neat trick!
    stack.call(add);

    let result = stack.pop::<u8>();

    // time for advanced calculus
    println!("add(1, 2) = {}", result);

    // stack should be empty now, lets check

    println!("Stack: {:?}", stack);
}

// some high level stuff right here
fn add((a, b): (u8, u8)) -> u8 {
    a + b
}
