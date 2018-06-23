#[cfg(feature = "ansi-colors")]
extern crate colored;
#[macro_use]
extern crate lazy_static;

pub mod token;
pub mod ast;
pub mod value;
pub mod scanner;
pub mod parser;
pub mod environment;
pub mod interpreter; // out of order

// TODO: move this somewhere else

use std::cmp;

pub fn print_parse_error(err: &parser::ParseError, source: &str) {
    // count lines
    let mut line_num = 0;
    let mut column = 0;
    {
        let mut cur = 0;
        for c in source.chars() {
            if cur >= err.token.start {
                break;
            }
            match c {
            	'\n' =>  {
                	line_num += 1;
	                column = 0;
	            }
	            '\t' => { column += 8 },
	            _ => { column += 1 }
            }
            cur += 1;
        }
    }

    let line_num_width = cmp::max((line_num as f64).log10() as usize, 5);

    let blame_width = err.token.end - err.token.start;

    let blame_line = source.lines().nth(line_num).unwrap();
    let blame_marker = format!("{}---", "^".repeat(blame_width));

    #[cfg(feature = "ansi-colors")]
    {
        use colored::*;

        println!("Parse error on line {}: {}", line_num + 1, err.message);
        println!(
            "--------------------------------------------------------------------------------"
        );
        println!(
            "{:^line_num_width$} | [...] ",
            " line",
            line_num_width = line_num_width
        );

        if line_num > 0 { // print some lines before
        	let start = if line_num > 4 {
        		line_num - 4
        	} else { 0 };

        	for i in start..line_num {
		        println!(
		            "{:line_num_width$} | {}",
		            i + 1,
		            source.lines().nth(i).unwrap(),
		            line_num_width = line_num_width
		        );	
        	}
        }

        println!(
            "{:line_num_width$} | {}",
            line_num + 1,
            blame_line.bold(),
            line_num_width = line_num_width
        );
        println!(
            "{:line_num_width$} : {}{} {}",
            "",
            " ".repeat(column),
            blame_marker.red(),
            err.message.bold(),
            line_num_width = line_num_width
        );
    }
    #[cfg(not(feature = "ansi-colors"))]
    {
        println!("Parse error on line {}: {}", line_num, err.message);
        println!(
            "--------------------------------------------------------------------------------"
        );
        println!(
            "{:^line_num_width$} | [...] ",
            " line",
            line_num_width = line_num_width
        );
        println!(
            "{:line_num_width$} | {}",
            line_num - 1,
            source.lines().nth(line_num - 2).unwrap(),
            line_num_width = line_num_width
        );
        println!(
            "{:line_num_width$} | {}",
            line_num,
            blame_line,
            line_num_width = line_num_width
        );
        println!(
            "{:line_num_width$} : {}{} {}",
            "",
            " ".repeat(column - 1),
            blame_marker,
            err.message,
            line_num_width = line_num_width
        );
    }
}
