//! Abstract Syntax Trees
//! and stuff

use std::fmt;

use token::Token;

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Literal::Number(ref val) => write!(f, "{}", val),
            &Literal::String(ref val) => write!(f, "\"{}\"", val),
            &Literal::Boolean(ref val) => write!(f, "{}", val),
            &Literal::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Unary {
        operator: Token,
        expr: Box<Expression>,
    },
    Binary {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
    Variable(String),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expression::Literal(ref literal) => write!(f, "{}", literal),
            &Expression::Unary {
                ref operator,
                ref expr,
            } => write!(f, "({} {})", operator, expr),
            &Expression::Binary {
                ref operator,
                ref left,
                ref right,
            } => write!(f, "({} {}, {})", operator, left, right),
            &Expression::Grouping(ref expr) => write!(f, "{}", expr),
            &Expression::Variable(ref id) => write!(f, "{}", id),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
	Expression(Box<Expression>),
	Print(Box<Expression>),
	VarDeclaration(String, Option<Box<Expression>>),
}