extern crate edda;

use std::io::{self, BufRead};

use edda::scanner::Scanner;
use edda::parser::Parser;
use edda::interpreter::Interpreter;

fn main() {
    let mut EoF = false;
    let stdin = io::stdin();
    let mut interpreter = Interpreter::new();

    for line in stdin.lock().lines() {
        let tokens = Scanner::new(line.unwrap()).scan_tokens();
        let statements = Parser::new(tokens).parse();
        interpreter.interpret(&statements).unwrap();
    }
}
