//! AST nodes that evaluate to values.

use std::fmt::{Display, Error, Formatter};

use crate::parser::{Parsable, ParseError, ParseResult};
use crate::token::{Token, TokenType};

/// Entry point for expression parsing
#[derive(Debug, Eq, PartialEq)]
pub enum Expression<'s> {
    Addition(Addition<'s>),
    Multiplication(Multiplication<'s>),
    Literal(Literal<'s>),
    Group(Grouping<'s>),
}

impl<'s> Parsable<'s> for Expression<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        Addition::try_parse(tokens)
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
pub struct Multiplication<'s>(Binary<'s>);

impl<'s> From<Multiplication<'s>> for Expression<'s> {
    fn from(expr: Multiplication<'s>) -> Self {
        Expression::Multiplication(expr)
    }
}

impl<'s> Parsable<'s> for Multiplication<'s> {
    type Node = Expression<'s>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (mut expr, mut tail) = primary(tokens)?;

        while peek_tokens!(tail, TokenType::Star, TokenType::Slash) {
            let (first, new_tail) = tail.split_first().expect("ran out of tokens when parsing");
            let operator = first.clone();
            let (rhs, new_tail) = primary(new_tail)?;

            expr = Multiplication(Binary::new(expr, operator, rhs)).into();

            tail = new_tail;
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
        _ => Literal::try_parse(tokens),
    }
}

// Grouped expressions, eq (1 + 1)
#[derive(Debug, Eq, PartialEq)]
pub struct Grouping<'s> {
    inner: Box<Expression<'s>>,
}

impl<'s> Grouping<'s> {
    pub fn new(inner: Expression<'s>) -> Grouping<'s> {
        Grouping {
            inner: Box::new(inner),
        }
    }
}

impl<'s> From<Grouping<'s>> for Expression<'s> {
    fn from(g: Grouping<'s>) -> Self {
        Expression::Group(g)
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
