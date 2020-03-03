//! AST nodes that evaluate to values.

use crate::ast::Expression;
use crate::parser::{Parsable, ParseResult};
use crate::token::{Token, TokenType};
use std::fmt::{Display, Error, Formatter};

/// Entry point for expression parsing
pub fn expression<'a, 's>(
    tokens: &'a [Token<'s>],
) -> ParseResult<'s, 'a, Box<dyn Expression + 's>> {
    Addition::try_parse(tokens).and_then(|(expr, tail)| Ok((expr as Box<dyn Expression>, tail)))
}

// Binary expressions
#[derive(Debug)]
pub struct Binary<'s> {
    pub lhs: Box<dyn Expression + 's>,
    pub operator: Token<'s>,
    pub rhs: Box<dyn Expression + 's>,
}

impl<'s> Display for Binary<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{} {} {}", self.lhs, self.operator, self.rhs)
    }
}

#[derive(Debug)]
pub struct Addition<'s>(Binary<'s>);

impl<'s> Display for Addition<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.0)
    }
}

impl<'s> Parsable<'s> for Addition<'s> {
    type Node = Box<dyn Expression + 's>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (mut expr, mut tail) = Multiplication::try_parse(tokens)
            .and_then(|(expr, tail)| Ok((expr as Box<dyn Expression>, tail)))?;

        while peek_tokens!(tail, TokenType::Plus, TokenType::Minus) {
            let (first, new_tail) = tail.split_at(1);
            let operator = first[0].clone();
            let (rhs, new_tail) = Multiplication::try_parse(new_tail)?;

            expr = Box::new(Addition(Binary {
                lhs: expr,
                operator,
                rhs,
            }));

            tail = new_tail;
        }

        Ok((expr, tail))
    }
}

impl<'s> Expression for Addition<'s> {}

#[derive(Debug)]
pub struct Multiplication<'s>(Binary<'s>);

impl<'s> Display for Multiplication<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.0)
    }
}

impl<'s> Parsable<'s> for Multiplication<'s> {
    type Node = Box<dyn Expression + 's>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (mut expr, mut tail): (Box<dyn Expression>, _) = Literal::try_parse(tokens)?;

        while peek_tokens!(tail, TokenType::Star, TokenType::Slash) {
            let (first, new_tail) = tail.split_at(1);
            let operator = first[0].clone();
            let (rhs, new_tail) = Literal::try_parse(new_tail)?;

            expr = Box::new(Multiplication(Binary {
                lhs: expr,
                operator,
                rhs,
            }));

            tail = new_tail;
        }

        Ok((expr, tail))
    }
}

impl<'s> Expression for Multiplication<'s> {}

// Primaries

#[derive(Debug)]
pub struct Literal<'s>(Token<'s>);

impl<'s> Display for Literal<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.0)
    }
}

impl<'s> Expression for Literal<'s> {}

impl<'s> Parsable<'s> for Literal<'s> {
    type Node = Box<dyn Expression + 's>;

    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self::Node> {
        let (value, tail) = match_tokens!(tokens, TokenType::Integer)?;
        Ok((Box::new(Literal(value)), tail))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::expressions::expression;
    use crate::ast::expressions::*;
    use crate::ast::Expression;
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
            format!(
                "{}",
                Box::new(Literal(Token {
                    t_type: TokenType::Integer,
                    offset: 0,
                    text: "1337"
                }))
            ),
            empty_tail,
        ));

        assert_eq!(
            expression(&tokens).and_then(|(ast, tail)| Ok((format!("{}", ast), tail))),
            expected
        )
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
            format!(
                "{}",
                Box::new(Addition(Binary {
                    lhs: Box::new(Literal(Token {
                        t_type: TokenType::Integer,
                        offset: 0,
                        text: "1"
                    })),
                    operator: Token {
                        t_type: TokenType::Plus,
                        offset: 2,
                        text: "+"
                    },
                    rhs: Box::new(Literal(Token {
                        t_type: TokenType::Integer,
                        offset: 4,
                        text: "2"
                    }))
                }))
            ),
            empty_tail,
        ));

        assert_eq!(
            expression(&tokens).and_then(|(ast, tail)| Ok((format!("{}", ast), tail))),
            expected
        )
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
            format!(
                "{}",
                Box::new(Multiplication(Binary {
                    lhs: Box::new(Literal(Token {
                        t_type: TokenType::Integer,
                        offset: 0,
                        text: "1"
                    })),
                    operator: Token {
                        t_type: TokenType::Plus,
                        offset: 2,
                        text: "*"
                    },
                    rhs: Box::new(Literal(Token {
                        t_type: TokenType::Integer,
                        offset: 4,
                        text: "2"
                    }))
                }))
            ),
            empty_tail,
        ));

        assert_eq!(
            expression(&tokens).and_then(|(ast, tail)| Ok((format!("{}", ast), tail))),
            expected
        )
    }
}
