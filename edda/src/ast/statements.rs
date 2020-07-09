//! Statement AST nodes.
//! Statements don't evaluate to values.

use crate::parser::{semicolon, Parsable, ParseError, ParseResult};
use crate::token::{Token, TokenType};

/// Entry point for statement parsing
#[derive(Debug, Eq, PartialEq)]
pub enum Statement<'s> {
    /// Return statement is statement because it does not evaluate to a value, but it also does
    /// evaluate to a value. I do not make the rules :shrug:
    Return(ReturnStmt<'s>),
}

impl<'s> Parsable<'s> for Statement<'s> {
    type Node = Statement<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        match tokens.first() {
            Some(Token {
                t_type: TokenType::Return,
                ..
            }) => ReturnStmt::try_parse(tokens),
            Some(t) => Err(Vec::from(ParseError {
                token: *t,
                expected: vec![TokenType::Return],
            })),
            None => panic!("should have atleast one token!!"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReturnStmt<'s> {
    token: Token<'s>,
}

impl<'s> From<ReturnStmt<'s>> for Statement<'s> {
    fn from(e: ReturnStmt<'s>) -> Self {
        Statement::Return(e)
    }
}

impl<'s> Parsable<'s> for ReturnStmt<'s> {
    type Node = Statement<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (token, tail) = match_tokens!(tokens, TokenType::Return)?;
        let (_, tail) = semicolon(tail)?;

        Ok((ReturnStmt { token }.into(), tail))
    }
}
