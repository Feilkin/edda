//! Statement AST nodes.
//! Statements don't evaluate to values.

use crate::parser::{semicolon, Parsable, ParseError, ParseResult};
use crate::token::{Token, TokenType};
use crate::Expression;

/// Entry point for statement parsing
#[derive(Debug, Eq, PartialEq)]
pub enum Statement<'s> {
    /// Return statement is statement because it does not evaluate to a value, but it also does
    /// evaluate to a value. I do not make the rules :shrug:
    Return(ReturnStmt<'s>),
    /// Function declaration.
    Fn(FnDecl<'s>),
    /// Variable declaration.
    VarDecl(VarDecl<'s>),
}

impl<'s> Parsable<'s> for Statement<'s> {
    type Node = Statement<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        match_and!(tokens,
            TokenType::Return => ReturnStmt::try_parse(tokens),
            TokenType::Fn => FnDecl::try_parse(tokens),
            TokenType::Let => VarDecl::try_parse(tokens),
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReturnStmt<'s> {
    pub value: Option<Box<Expression<'s>>>,
}

impl<'s> From<ReturnStmt<'s>> for Statement<'s> {
    fn from(e: ReturnStmt<'s>) -> Self {
        Statement::Return(e)
    }
}

impl<'s> Parsable<'s> for ReturnStmt<'s> {
    type Node = Statement<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (token, mut tail) = match_tokens!(tokens, TokenType::Return)?;

        // semicolon or expression semicolon
        let value = if peek_tokens!(tail, TokenType::Semicolon) {
            None
        } else {
            let (value, new_tail) = Expression::try_parse(tail)?;
            tail = new_tail;
            Some(Box::new(value))
        };

        let (_, tail) = semicolon(tail)?;
        Ok((ReturnStmt { value }.into(), tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FnDecl<'s> {
    name: Token<'s>,
    params: Vec<Token<'s>>,
    body: Box<Expression<'s>>,
    ret_type: Token<'s>,
}

impl<'s> From<FnDecl<'s>> for Statement<'s> {
    fn from(e: FnDecl<'s>) -> Self {
        Statement::Fn(e)
    }
}

impl<'s> Parsable<'s> for FnDecl<'s> {
    type Node = Statement<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (_, tail) = match_tokens!(tokens, TokenType::Fn)?;
        let (name, tail) = match_tokens!(tail, TokenType::Identifier)?;

        let (_, mut tail) = match_tokens!(tail, TokenType::LeftParen)?;

        let mut params = Vec::new();

        // TODO: types
        while peek_tokens!(tail, TokenType::Identifier) {
            let (token, new_tail) = match_tokens!(tail, TokenType::Identifier)?;
            tail = new_tail;

            params.push(token);

            // if next is ), break, else consume a ,
            // maybe not the best way to achieve (p1, p2, ..., pN) parsing
            if peek_tokens!(tail, TokenType::RightParen) {
                break;
            }

            let (_, new_tail) = match_tokens!(tail, TokenType::Comma)?;
            tail = new_tail;
        }

        let (_, tail) = match_tokens!(tail, TokenType::RightParen)?;
        let (_, tail) = match_tokens!(tail, TokenType::Arrow)?;

        let (ret_type, tail) = match_tokens!(tail, TokenType::Identifier)?;

        // TODO: this should actually be BlockExpression, I think?
        let (body, tail) = Expression::try_parse(tail)?;

        Ok((
            FnDecl {
                name,
                params,
                ret_type,
                body: Box::new(body),
            }
            .into(),
            tail,
        ))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct VarDecl<'s> {
    identifier: Token<'s>,
    initializer: Box<Expression<'s>>,
}

impl<'s> From<VarDecl<'s>> for Statement<'s> {
    fn from(s: VarDecl<'s>) -> Self {
        Statement::VarDecl(s)
    }
}

impl<'s> Parsable<'s> for VarDecl<'s> {
    type Node = Statement<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (_, tail) = match_tokens!(tokens, TokenType::Let)?;
        let (identifier, tail) = match_tokens!(tail, TokenType::Identifier)?;
        let (_, tail) = match_tokens!(tail, TokenType::Equal)?;

        let (initializer, tail) = Expression::try_parse(tail)?;

        let (_, tail) = semicolon(tail)?;

        Ok((
            VarDecl {
                identifier,
                initializer: Box::new(initializer),
            }
            .into(),
            tail,
        ))
    }
}
