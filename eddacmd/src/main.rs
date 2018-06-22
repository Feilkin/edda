extern crate edda;

use edda::scanner::{scan_tokens, ScanError};
use edda::parser::{parse_tokens, ParseError};

fn main() {
    let script = r#"
print 1 + (3 * 4)*(3 * 4); // quick maths
    testi
print "testi";
ripuli kakka neekeri;
print "multi\nline";
print -> "parse error";
print "valid line";
"#;

    println!("Running script:\n\n{}\n", script);

    let tokens = scan_tokens(script).unwrap();
    let statements = match parse_tokens(&tokens) {
        Ok(statements) => statements,
        Err(parse_errors) => {
            for parse_error in parse_errors {
                edda::print_parse_error(&parse_error, script);
            }
            return;
        }
    };
}
