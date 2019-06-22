extern crate edda;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use edda::interpreter::{Interpreter, RuntimeError};
use edda::parser::{parse_tokens, ParseError};
use edda::scanner::{scan_tokens, ScanError};

fn main() {
    println!("hello, world! :)");
}

fn run_ast_walker() {
    let args: Vec<_> = env::args().collect();
    let mut filename = "../examples/helloworld.edda";

    if args.len() > 1 {
        filename = &args[1];
    }

    let mut f = File::open(filename).expect("file not found");

    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("something went wrong reading the file");


    let tokens = scan_tokens(&script).unwrap();
    let statements = match parse_tokens(&tokens) {
        Ok(statements) => statements,
        Err(parse_errors) => {
            for parse_error in parse_errors {
                edda::print_parse_error(&parse_error, &script);
                println!("");
            }
            return;
        }
    };

    let mut interpreter = Interpreter::new();

    match interpreter.interpret(&statements) {
        Ok(_) => {}
        Err(err) => {
            println!("Runtime error: {:?}", err);
        }
    }
}
