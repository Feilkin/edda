#[macro_use]
extern crate lazy_static;
extern crate colored;

pub mod token;
pub mod ast;
pub mod value;
pub mod scanner;
pub mod parser;
//pub mod interpreter; // out of order

// TODO: move this somewhere else

use std::cmp;

pub fn print_parse_error(err: &parser::ParseError, source: &str) {
	use colored::*;

	// count lines
	let mut line_num = 1;
	let mut column = 1;
	{
		let mut cur = 0;
		for c in source.chars() {
			if cur >= err.token.start { break }
			if c == '\n' { line_num += 1; column = 0; }
			cur += 1;
			column += 1;
		}

		column += 1;
	}

	let line = source.lines().nth(line_num - 1).unwrap();
	let line_num_width = cmp::max((line_num as f64).log10() as usize, 5);

	let blame_width = err.token.end - err.token.start;

	println!("Parse error on line {}: {}", line_num, err.message);
	println!("--------------------------------------------------------------------------------");
	println!("{:^line_num_width$} | [...] ", "line", line_num_width = line_num_width);
	println!("{:line_num_width$} | {}", line_num - 1, source.lines().nth(line_num - 2).unwrap(), line_num_width = line_num_width);
	println!("{:line_num_width$} | {}", line_num, line.bold(), line_num_width = line_num_width);
	println!("{:line_num_width$} : {}{}", "", " ".repeat(column - 1), "^".repeat(blame_width).red().bold(), line_num_width = line_num_width);
}