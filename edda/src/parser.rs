//! Parser
//! parses and stuff

use std::slice::Iter;
use std::iter::Peekable;

use token::{Token, TokenType};
use ast::{Expression, Literal, Statement};

#[derive(Debug)]
pub struct ParseError<'a> {
    pub token: &'a Token,
    pub message: String,
}

pub fn parse_tokens(tokens: &Vec<Token>) -> Result<Vec<Statement>, Vec<ParseError>> {
    // early exit on empty token list (only contains EoF)
    if tokens.len() == 1 { return Ok(Vec::new()) }
    let mut errors = Vec::new();
    let mut error_mode = false;

    let mut statements = Vec::new();
    let mut token_iter = tokens.iter().peekable();

    loop {
        match declaration(&mut token_iter) {
            Ok(stmt) => statements.push(stmt),
            Err(err) => {
                errors.push(err);
                error_mode = true;
                synchronize(&mut token_iter);
            }
        };

        match token_iter.peek().unwrap().ttype {
            TokenType::EoF => break,
            _ => {}
        }
    }

    if error_mode {
        Err(errors)
    } else {
        Ok(statements)   
    }
}

fn synchronize<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> () {
    while let Some(token) = tokens.next() {
        match token.ttype {
            TokenType::EoF | TokenType::Semicolon => return,
            _ => {}
        }

        match tokens.peek().unwrap().ttype {
            // TODO: figure out what tokens will likely start a new statement
            TokenType::Let | TokenType::Print | TokenType::Return => return,
            _ => {}
        }
    }
}

fn declaration<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Statement, ParseError<'a>> {
    match tokens.peek().unwrap().ttype {
        TokenType::Let => {
            tokens.next(); // consume the Let
            match tokens.peek().unwrap().ttype {
                TokenType::Global => {
                    // global declaration
                    unimplemented!()
                },
                TokenType::Identifier(_) => {
                    variable_declaration(tokens)
                },
                _ => {
                    let next = tokens.next().unwrap();
                    Err(ParseError{
                        token: next,
                        message: format!("Unexpected {:?}, expected identifier or 'global'.", next)
                    })
                }
            }
        }
        _ => {
            statement(tokens)
        }
    }
}

fn variable_declaration<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Statement, ParseError<'a>> {
    let identifier = match tokens.next().unwrap().ttype {
        TokenType::Identifier(ref id) => { id.to_owned() },
        _ => panic!("expected identifier")
    };

    let initializer = match tokens.peek().unwrap().ttype {
        TokenType::Semicolon => {
            Ok(None)
        },
        TokenType::Equal => {
            tokens.next();
            Ok(Some(Box::new(expression(tokens)?)))
        },
        _ => {
            let next = tokens.next().unwrap();
            Err(ParseError {
                token: next,
                message: format!("Unexpected {:?}, expected ';' or '='.", next),
            })
        }
    }?;

    let next = tokens.next().unwrap();
    match next.ttype {
        TokenType::Semicolon => Ok(Statement::VarDeclaration(identifier, initializer)),
        _ => Err(ParseError {
            token: next,
            message: format!("Unexpected {:?}, expected semicolon ';'.", next.ttype),
        }),
    }
}

fn statement<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Statement, ParseError<'a>> {
    match tokens.peek().unwrap().ttype {
        TokenType::Print => {
            tokens.next();
            print_statement(tokens)
        }
        _ => expression_statement(tokens),
    }
}

fn print_statement<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Statement, ParseError<'a>> {
    let expr = expression(tokens)?;

    let next = tokens.next().unwrap();
    match next.ttype {
        TokenType::Semicolon => Ok(Statement::Print(Box::new(expr))),
        _ => Err(ParseError {
            token: next,
            message: format!("Unexpected {:?}, expected semicolon ';'.", next.ttype),
        }),
    }
}

fn expression_statement<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Statement, ParseError<'a>> {
    let expr = expression(tokens)?;

    let next = tokens.next().unwrap();
    match next.ttype {
        TokenType::Semicolon => Ok(Statement::Print(Box::new(expr))),
        _ => Err(ParseError {
            token: next,
            message: format!("Unexpected {:?}, expected semicolon ';'.", next.ttype),
        }),
    }
}

fn expression<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError<'a>> {
    equality(tokens)
}

fn equality<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError<'a>> {
    let mut left = comparison(tokens)?;

    while match tokens.peek().unwrap().ttype {
        TokenType::BangEqual | TokenType::EqualEqual => true,
        _ => false,
    } {
        let operator = tokens.next().unwrap().clone();
        let right = comparison(tokens)?;
        left = Expression::Binary {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        };
    }

    Ok(left)
}

fn comparison<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError<'a>> {
    let mut left = addition(tokens)?;

    while match tokens.peek().unwrap().ttype {
        TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual => {
            true
        }
        _ => false,
    } {
        let operator = tokens.next().unwrap().clone();
        let right = addition(tokens)?;
        left = Expression::Binary {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        };
    }

    Ok(left)
}

fn addition<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError<'a>> {
    let mut left = multiplication(tokens)?;

    while match tokens.peek().unwrap().ttype {
        TokenType::Plus | TokenType::Minus => true,
        _ => false,
    } {
        let operator = tokens.next().unwrap().clone();
        let right = multiplication(tokens)?;
        left = Expression::Binary {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        };
    }

    Ok(left)
}

fn multiplication<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Expression, ParseError<'a>> {
    let mut left = unary(tokens)?;

    while match tokens.peek().unwrap().ttype {
        TokenType::Slash | TokenType::Star => true,
        _ => false,
    } {
        let operator = tokens.next().unwrap().clone();
        let right = unary(tokens)?;
        left = Expression::Binary {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        };
    }

    Ok(left)
}

fn unary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError<'a>> {
    match tokens.peek().unwrap().ttype {
        TokenType::Minus | TokenType::Bang => {
            let operator = tokens.next().unwrap().clone();
            let expr = unary(tokens)?;
            return Ok(Expression::Unary {
                operator,
                expr: Box::new(expr),
            });
        }
        _ => {}
    }

    primary(tokens)
}

fn primary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError<'a>> {
    let token = tokens.next().unwrap();

    match token.ttype {
        TokenType::Identifier(ref id) => Ok(Expression::Variable(id.clone())),
        TokenType::Number(val) => Ok(Expression::Literal(Literal::Number(val))),
        TokenType::String(ref val) => Ok(Expression::Literal(Literal::String(val.clone()))),
        TokenType::False => Ok(Expression::Literal(Literal::Boolean(false))),
        TokenType::True => Ok(Expression::Literal(Literal::Boolean(true))),
        TokenType::Nil => Ok(Expression::Literal(Literal::Nil)),
        TokenType::LeftParen => {
            let expr = expression(tokens)?;

            let next = tokens.next().unwrap();
            match next.ttype {
                TokenType::RightParen => Ok(Expression::Grouping(Box::new(expr))),
                _ => Err(ParseError {
                    token: next,
                    message: format!(
                        "Unexpected {:?}, expected closing parenthesis ')'.",
                        next.ttype
                    ),
                }),
            }
        }
        _ => Err(ParseError {
            token: token,
            message: format!("Unexpected {:?}.", token.ttype),
        }),
    }
}
