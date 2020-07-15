//! Parser types, traits, and utility functions

use std::fmt::{Error as FmtError, Write};

use crate::ast::statements::Statement;
use crate::token::{Token, TokenType};
use crate::Error;
use itertools::Itertools;

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError<'s> {
    pub token: Token<'s>,
    pub expected: Vec<TokenType>,
}

impl<'s> ParseError<'s> {
    // TODO: source should come from the token?
    pub fn display_err(&self, source: &str) -> Result<String, FmtError> {
        // TODO: implement Display on tokentype
        let error_msg = if (self.expected.len() == 1) {
            format!(
                "Unexpected {}, expected {:?} instead!",
                self.token, self.expected[0]
            )
        } else {
            format!(
                "Unexpected {}, expected one of [ {} ] instead!",
                self.token,
                self.expected.iter().map(|e| format!("{:?}", e)).join(", ")
            )
        };

        // count lines
        let mut line_num = 0;
        let mut column = 0;
        {
            let mut cur = 0;
            for c in source.chars() {
                if cur >= self.token.offset {
                    break;
                }
                match c {
                    '\n' => {
                        line_num += 1;
                        column = 0;
                    }
                    '\t' => column += 8,
                    _ => column += 1,
                }
                cur += 1;
            }
        }
        let line_num_width = std::cmp::max((line_num as f64).log10() as usize, 5);

        let blame_width = self.token.text.len();

        let blame_line = source.lines().nth(line_num).unwrap();
        let blame_marker = format!("{}---", "^".repeat(blame_width));

        let mut buffer = String::new();
        use colored::*;

        writeln!(
            buffer,
            "Parse error on line {}: {}",
            line_num + 1,
            error_msg
        )?;
        writeln!(
            buffer,
            "--------------------------------------------------------------------------------"
        )?;
        writeln!(
            buffer,
            "{:^line_num_width$} | [...] ",
            " line",
            line_num_width = line_num_width
        )?;

        if line_num > 0 {
            // print some lines before
            let start = if line_num > 4 { line_num - 4 } else { 0 };

            for i in start..line_num {
                writeln!(
                    buffer,
                    "{:line_num_width$} | {}",
                    i + 1,
                    source.lines().nth(i).unwrap(),
                    line_num_width = line_num_width
                )?;
            }
        }

        writeln!(
            buffer,
            "{:line_num_width$} | {}",
            line_num + 1,
            blame_line.bold(),
            line_num_width = line_num_width
        )?;
        writeln!(
            buffer,
            "{:line_num_width$} : {}{} {}",
            "",
            " ".repeat(column),
            blame_marker.red(),
            error_msg.bold(),
            line_num_width = line_num_width
        )?;

        Ok(buffer)
    }
}

pub type ParseResult<'s, 'a, R> = Result<(R, &'a [Token<'s>]), Vec<ParseError<'s>>>;

impl<'s> From<ParseError<'s>> for Vec<ParseError<'s>> {
    fn from(err: ParseError<'s>) -> Self {
        vec![err]
    }
}

// Parsa :-DD
pub trait Parsable<'s> {
    type Node;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node>;
}

macro_rules! match_tokens {
    ($token_stream:ident, $($valid:path),+) => {
        // It's OK to unwrap here, because we should always catch EOF
        // before tokens run out
        match $token_stream.first().unwrap() {
            $(token @ Token {
                t_type: $valid,
                ..
            })|+ => Ok((token.clone(), &$token_stream[1..])),
            unexpected @ _ => Err(crate::parser::ParseError {
                token: unexpected.clone(),
                expected: vec![$($valid,)+]
            }),
        }
    };
}

macro_rules! peek_tokens {
    ($token_stream:ident, $($valid:path),+) => {
        // It's OK to unwrap here, because we should always catch EOF
        // before tokens run out
        match $token_stream.first().unwrap() {
            $(Token {
                t_type: $valid,
                ..
            })|+ => true,
            _ => false,
        }
    };
}

macro_rules! match_and {
    ($token_stream:ident, $($valid:path => $body:expr),+ $(,)?) => {
        match $token_stream.first().unwrap() {
            $(token @ Token {
                t_type: $valid,
                ..
            } => $body,)+
            unexpected @ _ => Err(vec![crate::parser::ParseError {
                token: unexpected.clone(),
                expected: vec![$($valid,)+]
            }])
        }
    };
}

pub fn semicolon<'a, 's>(
    tokens: &'a [Token<'s>],
) -> Result<((), &'a [Token<'s>]), Vec<ParseError<'s>>> {
    match tokens.first().unwrap() {
        Token {
            t_type: TokenType::Semicolon,
            ..
        } => Ok(((), &tokens[1..])),
        unexpected @ _ => Err(vec![ParseError {
            token: *unexpected,
            expected: vec![TokenType::Semicolon],
        }]),
    }
}

pub fn script<'a, 's>(source: &'a [Token<'s>]) -> ParseResult<'s, 'a, Vec<Statement<'s>>> {
    let mut tail = source;
    let mut statements = Vec::new();

    while !peek_tokens!(tail, TokenType::Eof) {
        let (stmt, new_tail) = Statement::try_parse(tail)?;
        tail = new_tail;

        statements.push(stmt);
    }

    Ok((statements, tail))
}
