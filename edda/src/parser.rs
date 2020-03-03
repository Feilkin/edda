//! Parser types, traits, and utility functions

use crate::token::{Token, TokenType};

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError<'s> {
    pub token: Token<'s>,
    pub expected: Vec<TokenType>,
}

pub type ParseResult<'s, 'a, R> = Result<(Box<R>, &'a [Token<'s>]), Vec<ParseError<'s>>>;

impl<'s> From<ParseError<'s>> for Vec<ParseError<'s>> {
    fn from(err: ParseError<'s>) -> Self {
        vec![err]
    }
}

// Parsa :-DD
pub trait Parsable<'s> {
    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self>;
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

pub fn semicolon<'s>(tokens: &[Token<'s>]) -> Result<(), Vec<ParseError<'s>>> {
    match tokens.first().unwrap() {
        Token {
            t_type: TokenType::Semicolon,
            ..
        } => Ok(()),
        unexpected @ _ => Err(vec![ParseError {
            token: *unexpected,
            expected: vec![TokenType::Semicolon],
        }]),
    }
}
