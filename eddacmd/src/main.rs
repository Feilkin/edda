extern crate edda;

use edda::scanner::{scan_tokens, ScanError};
use edda::parser::{parse_tokens, ParseError};

fn main() {
	let script = r#"
print 1 + (3 * 4)*(3 * 4); // quick maths
print "testi";
print "multi\nline";
print -> "parse error"
"#;

	println!("Running script:\n\n{}\n", script);

    let tokens = scan_tokens(script).unwrap();
    let statements = match parse_tokens(&tokens) {
    	Ok(statements) => statements,
    	Err(parse_error) => {
    		edda::print_parse_error(&parse_error, script);
    		return
    	},
    };
}
