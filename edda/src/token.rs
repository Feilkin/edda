//! Token type definition

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;

        match self {
            &Number(val) => write!(f, "{}", val),
            &String(ref val) => write!(f, "\"{}\"", val),
            &LeftParen => write!(f, "("),
            &RightParen => write!(f, ")"),
            &LeftBrace => write!(f, "{{"),
            &RightBrace => write!(f, "}}"),
            &Comma => write!(f, ","),
            &Dot => write!(f, "."),
            &DotDot => write!(f, ".."),
            &Semicolon => write!(f, ";"),
            &Plus => write!(f, "+"),
            &Minus => write!(f, "-"),
            &Star => write!(f, "*"),
            &Slash => write!(f, "Slash"),
            &Caret => write!(f, "^"),
            &Bang => write!(f, "!"),
            &BangEqual => write!(f, "!="),
            &Equal => write!(f, "="),
            &EqualEqual => write!(f, "=="),
            &LessEqual => write!(f, "<="),
            &GreaterEqual => write!(f, ">="),
            &Less => write!(f, "<"),
            &Greater => write!(f, ">"),
            &Arrow => write!(f, "->"),
            &FatArrow => write!(f, "=>"),
            &Identifier(ref id) => write!(f, "{}", id),
            &Let => write!(f, "let"),
            &And => write!(f, "and"),
            &Or => write!(f, "or"),
            &For => write!(f, "for"),
            &In => write!(f, "in"),
            &While => write!(f, "while"),
            &Print => write!(f, "print"),
            &True => write!(f, "true"),
            &False => write!(f, "false"),
            &Nil => write!(f, "nil"),
            &EoF => write!(f, ""),
        }
    }
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("let", Token::Let);
        m.insert("and", Token::And);
        m.insert("or", Token::Or);
        m.insert("for", Token::For);
        m.insert("in", Token::In);
        m.insert("while", Token::While);
        m.insert("true", Token::True);
        m.insert("false", Token::False);
        m.insert("nil", Token::Nil);
        m.insert("print", Token::Print);
        m
    };
}
