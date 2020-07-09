use std::env;
use std::fs::File;
use std::io::prelude::*;

use edda::{compile, parser, scan, Chunk, Error as EddaError, Expression, OpCode, Vm, VmState};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<_> = env::args().collect();
    let mut filename = "examples/fibonacci.edda";

    if args.len() > 1 {
        filename = &args[1];
    }

    let mut f = File::open(filename).expect("file not found");

    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("something went wrong reading the file");

    let tokens = scan(&script)?;

    use edda::parser::Parsable;
    let ast = match parser::script(&tokens) {
        Ok((ast, _)) => ast,
        Err(parse_errs) => {
            for err in parse_errs {
                println!("{}", err.display_err(&script)?);
            }

            return Ok(());
        }
    };
    let mut chunk = compile(ast, Chunk::new())?;

    println!("{:?}", &chunk);

    let mut vm = Vm::new(chunk);
    println!("{:?}", &vm);

    let ret_val = loop {
        match vm.run() {
            VmState::Running(new_vm) => {
                vm = new_vm;
                println!("{:?}", &vm);
            }
            VmState::Finished(val) => break val,
        }
    };

    println!("Return value: {}", ret_val);

    Ok(())
}
