//! Token type definition

use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Number(f64),
    String(String),
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
    Caret,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Arrow,
    FatArrow,
    Identifier(String),
    Let,
    And,
    Or,
    For,
    In,
    While,
    Print,
    True,
    False,
    Nil,
    EoF,
    Return,
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("let", TokenType::Let);
        m.insert("and", TokenType::And);
        m.insert("or", TokenType::Or);
        m.insert("for", TokenType::For);
        m.insert("in", TokenType::In);
        m.insert("while", TokenType::While);
        m.insert("true", TokenType::True);
        m.insert("false", TokenType::False);
        m.insert("nil", TokenType::Nil);
        m.insert("print", TokenType::Print);
        m.insert("return", TokenType::Return);
        m
    };
}
