//! AST nodes that evaluate to values.

use std::fmt::{Display, Error, Formatter};

use crate::ast::statements::Statement;
use crate::parser::{Parsable, ParseError, ParseResult};
use crate::token::{Token, TokenType};

/// Entry point for expression parsing
#[derive(Debug, Eq, PartialEq)]
pub enum Expression<'s> {
    If(IfExpr<'s>),
    Equality(Equality<'s>),
    Comparison(Comparison<'s>),
    Addition(Addition<'s>),
    Multiplication(Multiplication<'s>),
    Call(CallExpr<'s>),
    Literal(Literal<'s>),
    Variable(Variable<'s>),
    Group(Grouping<'s>),
    Block(BlockExpr<'s>),
}

impl<'s> Parsable<'s> for Expression<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        match tokens.first() {
            Some(Token {
                t_type: TokenType::If,
                ..
            }) => IfExpr::try_parse(tokens),
            _ => Equality::try_parse(tokens),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct IfExpr<'s> {
    pub condition: Box<Expression<'s>>,
    pub body: Box<Expression<'s>>,
    pub else_body: Box<Expression<'s>>,
}

impl<'s> From<IfExpr<'s>> for Expression<'s> {
    fn from(e: IfExpr<'s>) -> Self {
        Expression::If(e)
    }
}

impl<'s> Parsable<'s> for IfExpr<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (_, tail) = match_tokens!(tokens, TokenType::If)?;
        let (condition, tail) = Expression::try_parse(tail)?;
        let (body, tail) = BlockExpr::try_parse(tail)?;
        let (_, tail) = match_tokens!(tail, TokenType::Else)?;

        // allow chaining if after else
        let (else_body, tail) = if peek_tokens!(tail, TokenType::If) {
            IfExpr::try_parse(tail)
        } else {
            BlockExpr::try_parse(tail)
        }?;
        Ok((
            IfExpr {
                condition: Box::new(condition),
                body: Box::new(body),
                else_body: Box::new(else_body),
            }
            .into(),
            tail,
        ))
    }
}

// Binary expressions
#[derive(Debug, Eq, PartialEq)]
pub struct Binary<'s> {
    pub lhs: Box<Expression<'s>>,
    pub operator: Token<'s>,
    pub rhs: Box<Expression<'s>>,
}

impl<'s> Binary<'s> {
    pub fn new(lhs: Expression<'s>, operator: Token<'s>, rhs: Expression<'s>) -> Binary<'s> {
        Binary {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        }
    }
}

// equality + comparison

#[derive(Debug, Eq, PartialEq)]
pub struct Equality<'s>(pub Binary<'s>);

impl<'s> From<Equality<'s>> for Expression<'s> {
    fn from(expr: Equality<'s>) -> Self {
        Expression::Equality(expr)
    }
}

impl<'s> Parsable<'s> for Equality<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (mut expr, mut tail) = Comparison::try_parse(tokens)?;

        while peek_tokens!(tail, TokenType::EqualEqual) {
            let (first, new_tail) = tail.split_first().expect("ran out of tokens when parsing");
            let operator = first.clone();
            let (rhs, new_tail) = Comparison::try_parse(new_tail)?;

            expr = Equality(Binary::new(expr, operator, rhs)).into();

            tail = new_tail;
        }

        Ok((expr, tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Comparison<'s>(pub Binary<'s>);

impl<'s> From<Comparison<'s>> for Expression<'s> {
    fn from(expr: Comparison<'s>) -> Self {
        Expression::Comparison(expr)
    }
}

impl<'s> Parsable<'s> for Comparison<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (mut expr, mut tail) = Addition::try_parse(tokens)?;

        while peek_tokens!(
            tail,
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual
        ) {
            let (first, new_tail) = tail.split_first().expect("ran out of tokens when parsing");
            let operator = first.clone();
            let (rhs, new_tail) = Addition::try_parse(new_tail)?;

            expr = Comparison(Binary::new(expr, operator, rhs)).into();

            tail = new_tail;
        }

        Ok((expr, tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Addition<'s>(pub Binary<'s>);

impl<'s> From<Addition<'s>> for Expression<'s> {
    fn from(expr: Addition<'s>) -> Self {
        Expression::Addition(expr)
    }
}

impl<'s> Parsable<'s> for Addition<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (mut expr, mut tail) = Multiplication::try_parse(tokens)?;

        while peek_tokens!(tail, TokenType::Plus, TokenType::Minus) {
            let (first, new_tail) = tail.split_first().expect("ran out of tokens while parsing");
            let operator = first.clone();
            let (rhs, new_tail) = Multiplication::try_parse(new_tail)?;

            expr = Addition(Binary::new(expr, operator, rhs)).into();

            tail = new_tail;
        }

        Ok((expr, tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Multiplication<'s>(pub Binary<'s>);

impl<'s> From<Multiplication<'s>> for Expression<'s> {
    fn from(expr: Multiplication<'s>) -> Self {
        Expression::Multiplication(expr)
    }
}

impl<'s> Parsable<'s> for Multiplication<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (mut expr, mut tail) = CallExpr::try_parse(tokens)?;

        while peek_tokens!(tail, TokenType::Star, TokenType::Slash) {
            let (first, new_tail) = tail.split_first().expect("ran out of tokens when parsing");
            let operator = first.clone();
            let (rhs, new_tail) = CallExpr::try_parse(new_tail)?;

            expr = Multiplication(Binary::new(expr, operator, rhs)).into();

            tail = new_tail;
        }

        Ok((expr, tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct CallExpr<'s> {
    pub callee: Box<Expression<'s>>,
    pub arguments: Vec<Expression<'s>>,
}

impl<'s> From<CallExpr<'s>> for Expression<'s> {
    fn from(expr: CallExpr<'s>) -> Self {
        Expression::Call(expr)
    }
}

impl<'s> Parsable<'s> for CallExpr<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (mut expr, mut tail) = primary(tokens)?;

        while peek_tokens!(tail, TokenType::LeftParen) {
            // skip opening paren
            tail = &tail[1..];
            let mut arguments = Vec::new();

            while !peek_tokens!(tail, TokenType::RightParen) {
                let (arg, new_tail) = Expression::try_parse(tail)?;
                arguments.push(arg);
                tail = new_tail;

                // if next is ), break, else consume a ,
                // maybe not the best way to achieve (e1, e2, ..., eN) parsing
                if peek_tokens!(tail, TokenType::RightParen) {
                    break;
                }

                let (_, new_tail) = match_tokens!(tail, TokenType::Comma)?;
                tail = new_tail;
            }

            let (_, new_tail) = match_tokens!(tail, TokenType::RightParen)?;
            tail = new_tail;

            expr = CallExpr {
                callee: Box::new(expr),
                arguments,
            }
            .into();
        }

        Ok((expr, tail))
    }
}

// Primaries
fn primary<'a, 's>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Expression<'s>> {
    match tokens[0] {
        Token {
            t_type: TokenType::LeftParen,
            ..
        } => Grouping::try_parse(tokens),
        Token {
            t_type: TokenType::Identifier,
            ..
        } => Variable::try_parse(tokens),
        Token {
            t_type: TokenType::LeftBrace,
            ..
        } => BlockExpr::try_parse(tokens),
        _ => Literal::try_parse(tokens),
    }
}

// Grouped expressions, eq (1 + 1)
#[derive(Debug, Eq, PartialEq)]
pub struct Grouping<'s> {
    pub inner: Box<Expression<'s>>,
}

impl<'s> Grouping<'s> {
    pub fn new(inner: Expression<'s>) -> Grouping<'s> {
        Grouping {
            inner: Box::new(inner),
        }
    }
}

impl<'s> From<Grouping<'s>> for Expression<'s> {
    fn from(e: Grouping<'s>) -> Self {
        Expression::Group(e)
    }
}

impl<'s> Parsable<'s> for Grouping<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (_, tail) = match_tokens!(tokens, TokenType::LeftParen)?;
        let (inner, tail) = Expression::try_parse(tail)?;
        let (_, tail) = match_tokens!(tail, TokenType::RightParen)?;

        Ok((Grouping::new(inner).into(), tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct BlockExpr<'s> {
    pub statements: Vec<Statement<'s>>,
    pub ret: Box<Expression<'s>>,
}

impl<'s> BlockExpr<'s> {
    pub fn new(statements: Vec<Statement<'s>>, ret: Expression<'s>) -> BlockExpr<'s> {
        BlockExpr {
            statements,
            ret: Box::new(ret),
        }
    }
}

impl<'s> From<BlockExpr<'s>> for Expression<'s> {
    fn from(e: BlockExpr<'s>) -> Self {
        Expression::Block(e)
    }
}

impl<'s> Parsable<'s> for BlockExpr<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (_, mut tail) = match_tokens!(tokens, TokenType::LeftBrace)?;

        let mut statements = Vec::new();

        let (ret, tail) = loop {
            // because we can't tell expression from statement before we actually try parsing it,
            // we have to do this stupid try match thing
            match Statement::try_parse(tail) {
                Ok((stmt, new_tail)) => {
                    statements.push(stmt);
                    tail = new_tail;
                }
                Err(mut stmt_errs) => {
                    // this could be the tailing expression
                    break Expression::try_parse(tail).or_else(|err| {
                        stmt_errs.extend(err);
                        Err(stmt_errs)
                    })?;
                }
            }
        };

        let (_, tail) = match_tokens!(tail, TokenType::RightBrace)?;

        Ok((BlockExpr::new(statements, ret).into(), tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Literal<'s>(Token<'s>);

impl<'s> Literal<'s> {
    pub fn token(&self) -> &Token<'s> {
        &self.0
    }
}

impl<'s> From<Literal<'s>> for Expression<'s> {
    fn from(expr: Literal<'s>) -> Self {
        Expression::Literal(expr)
    }
}

impl<'s> Parsable<'s> for Literal<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (value, tail) = match_tokens!(tokens, TokenType::Integer)?;
        Ok((Literal(value).into(), tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Variable<'s>(pub Token<'s>);

impl<'s> Variable<'s> {
    pub fn token(&self) -> &Token<'s> {
        &self.0
    }
}

impl<'s> From<Variable<'s>> for Expression<'s> {
    fn from(expr: Variable<'s>) -> Self {
        Expression::Variable(expr)
    }
}

impl<'s> Parsable<'s> for Variable<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (value, tail) = match_tokens!(tokens, TokenType::Identifier)?;
        Ok((Variable(value).into(), tail))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::expressions::*;
    use crate::parser::Parsable;
    use crate::scan;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_int_literal() {
        let tokens = scan("1337").unwrap();
        let empty_tail: &[Token] = &[Token {
            t_type: TokenType::Eof,
            offset: 4,
            text: "",
        }];

        let expected = Ok((
            Literal(Token {
                t_type: TokenType::Integer,
                offset: 0,
                text: "1337",
            })
            .into(),
            empty_tail,
        ));

        assert_eq!(Expression::try_parse(&tokens), expected)
    }

    #[test]
    fn test_binary_add() {
        let tokens = scan("1 + 2").unwrap();
        let empty_tail: &[Token] = &[Token {
            t_type: TokenType::Eof,
            offset: 5,
            text: "",
        }];

        let expected = Ok((
            Expression::Addition(Addition(Binary::new(
                Literal(Token {
                    t_type: TokenType::Integer,
                    offset: 0,
                    text: "1",
                })
                .into(),
                Token {
                    t_type: TokenType::Plus,
                    offset: 2,
                    text: "+",
                },
                Literal(Token {
                    t_type: TokenType::Integer,
                    offset: 4,
                    text: "2",
                })
                .into(),
            ))),
            empty_tail,
        ));

        assert_eq!(Expression::try_parse(&tokens), expected)
    }

    #[test]
    fn test_binary_multiply() {
        let tokens = scan("1 * 2").unwrap();
        let empty_tail: &[Token] = &[Token {
            t_type: TokenType::Eof,
            offset: 5,
            text: "",
        }];

        let expected = Ok((
            Expression::Multiplication(Multiplication(Binary::new(
                Literal(Token {
                    t_type: TokenType::Integer,
                    offset: 0,
                    text: "1",
                })
                .into(),
                Token {
                    t_type: TokenType::Star,
                    offset: 2,
                    text: "*",
                },
                Literal(Token {
                    t_type: TokenType::Integer,
                    offset: 4,
                    text: "2",
                })
                .into(),
            ))),
            empty_tail,
        ));

        assert_eq!(Expression::try_parse(&tokens), expected)
    }
}
