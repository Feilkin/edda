//! AST nodes that evaluate to values.

use crate::ast::Expression;
use crate::parser::{Parsable, ParseResult};
use crate::token::{Token, TokenType};
use std::fmt::{Display, Formatter, Error};

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

impl<'s> Expression for Binary<'s> {}

// TODO: very very hack
impl<'s> PartialEq for Binary<'s> {
    fn eq(&self, other: &Self) -> bool {
        if self.operator != other.operator { return false; }
        if format!("{}", self) != format!("{}", other) { return false; }
        true
    }
}

impl<'s> Parsable<'s> for Binary<'s> {
    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self> {
        let (lhs, tail) = Literal::try_parse(tokens)?;

        let (operator, tail) = match_tokens!(
            tail,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Slash
        )?;

        let (rhs, tail) = Literal::try_parse(tail)?;

        Ok((Box::new(Binary { lhs, operator, rhs }), tail))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Literal<'s>(Token<'s>);

impl<'s> Display for Literal<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.0)
    }
}

impl<'s> Expression for Literal<'s> {}

impl<'s> Parsable<'s> for Literal<'s> {
    fn try_parse<'a>(tokens: &'a [Token<'s>]) -> ParseResult<'s, 'a, Self> {
        let (value, tail) = match_tokens!(tokens, TokenType::Integer)?;
        Ok((Box::new(Literal(value)), tail))
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
            text: ""
        }];

        assert_eq!(
            Literal::try_parse(&tokens),
            Ok((Box::new(Literal(Token {
                t_type: TokenType::Integer,
                offset: 0,
                text: "1337"
            })), empty_tail))
        )
    }

    #[test]
    fn test_binary_add() {
        let tokens = scan("1 + 2").unwrap();
        let empty_tail: &[Token] = &[Token {
            t_type: TokenType::Eof,
            offset: 5,
            text: ""
        }];

        assert_eq!(
            Binary::try_parse(&tokens),
            Ok((Box::new(Binary {
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
            }), empty_tail))
        )
    }
}
