//! token definitions

use std::fmt::{Display, Error, Formatter};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Token<'s> {
    pub t_type: TokenType,
    pub offset: usize,
    pub text: &'s str,
}

impl<'s> Display for Token<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.text)
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenType {
    Integer,
    Float,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    DotDot,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    LineComment,
    Caret,
    Bang,
    BangEqual,
    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Arrow,
    FatArrow,
    Identifier,
    Let,
    And,
    Or,
    For,
    In,
    While,
    Print,
    True,
    False,
    If,
    Else,
    Return,
    Eof,
}
