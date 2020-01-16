extern crate edda;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use edda::parser::{parse_tokens, ParseError};
use edda::scanner::{scan_tokens, ScanError};
use edda::vm::{Compiler, Vm};

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut filename = "examples/helloworld.edda";

    if args.len() > 1 {
        filename = &args[1];
    }

    let mut f = File::open(filename).expect("file not found");

    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("something went wrong reading the file");
    
    let tokens = scan_tokens(&script).unwrap();
    let ast = match parse_tokens(&tokens) {
        Ok(statements) => statements,
        Err(parse_errors) => {
            for parse_error in parse_errors {
                edda::print_parse_error(&parse_error, &script);
                println!("");
            }
            return;
        }
    };

    // compile ast to bytecode
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&ast);

    let mut vm = Vm::new();
    vm.enable_tracing();
    let val = vm.interpret::<f64>(&chunk).unwrap();

    println!("val: {}", val);
}