extern crate edda;

use edda::scanner::{scan_tokens, ScanError};
use edda::parser::{parse_tokens, ParseError};
use edda::interpreter::{Interpreter, RuntimeError};

fn main() {
    let script = r#"
print 1 + (3 * 4)*(3 * 4); // quick maths
print "testi";
print "multi\nline";

let a = "testi";
print a;

let b = 3;

print b + 3;

print a + b;
"#;

    println!("Running script:\n\n{}\n", script);

    let tokens = scan_tokens(script).unwrap();
    let statements = match parse_tokens(&tokens) {
        Ok(statements) => statements,
        Err(parse_errors) => {
            for parse_error in parse_errors {
                edda::print_parse_error(&parse_error, script);
                println!("");
            }
            return;
        }
    };

    let mut interpreter = Interpreter::new();

    match interpreter.interpret(&statements) {
    	Ok(_) => {},
    	Err(err) => {
    		println!("Runtime error: {:?}", err);
    	}
    }
}
