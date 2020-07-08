use std::env;
use std::fs::File;
use std::io::prelude::*;

use edda::{compile, scan, Chunk, Error as EddaError, Expression, Vm};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<_> = env::args().collect();
    let mut filename = "examples/helloworld.edda";

    if args.len() > 1 {
        filename = &args[1];
    }

    let mut f = File::open(filename).expect("file not found");

    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("something went wrong reading the file");

    let tokens = scan(&script)?;

    use edda::parser::Parsable;
    let ast = match Expression::try_parse(&tokens) {
        Ok((ast, _)) => ast,
        Err(parse_errs) => {
            for err in parse_errs {
                println!("{}", err.display_err(&script)?);
            }

            return Ok(());
        }
    };
    let mut chunk = compile(ast, Chunk::new())?;
    let vm = Vm::new(chunk);
    vm.run();

    Ok(())
}
