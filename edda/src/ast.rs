//! Abstract Syntax Trees
//! and stuff

use std::fmt;

use crate::token::Token;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    FunctionDeclaration(Vec<String>, Box<Expression>),
    FunctionCall(Box<Expression>, Vec<Expression>),
    BlockExpression(Vec<Statement>, Box<Expression>),
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
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Box<Expression>),
    Print(Box<Expression>),
    VarDeclaration(String, Option<Box<Expression>>),
    GlobalDeclaration(String, Box<Expression>),
    BlockStatement(Vec<Statement>),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    For(Vec<String>, Box<Expression>, Box<Statement>),
}
