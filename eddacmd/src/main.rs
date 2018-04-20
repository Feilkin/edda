extern crate edda;

use edda::scanner::Scanner;
use edda::parser::Parser;
use edda::interpreter::Interpreter;

fn main() {
	let script = r#"
print 1 + (3 * 4)*(3 * 4); // quick maths
print "testi";
print "multi\nline";
"#;

	println!("Script: `{}`", script);

    let tokens = Scanner::new(script.to_owned()).scan_tokens();
    let statements = Parser::new(tokens).parse();

    let mut interpreter = Interpreter::new();
    println!("Result: `{}`", interpreter.interpret(&statements).unwrap());
}
