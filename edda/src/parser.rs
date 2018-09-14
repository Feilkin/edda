//! Parser
//! parses and stuff

//        `\.      ,/'
//         |\\____//|
//         )/_ `' _\(
//        ,'/-`__'-\`\
//        /. (_><_) ,\
//        ` )/`--'\(`'  t u f t
//          `      '


use std::iter::Peekable;
use std::slice::Iter;

use ast::{Expression, Literal, Statement};
use token::{Token, TokenType};

#[derive(Debug)]
pub struct ParseError {
    pub token: Token,
    pub message: String,
}

pub fn parse_tokens(tokens: &Vec<Token>) -> Result<Vec<Statement>, Vec<ParseError>> {
    // early exit on empty token list (only contains EoF)
    if tokens.len() == 1 {
        return Ok(Vec::new());
    }
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

        match token_iter.peek() {
            Some(ref token) => match token.ttype {
                TokenType::EoF => break,
                _ => {}
            },
            None => break,
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

fn declaration<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Statement, ParseError> {
    match tokens.peek().unwrap().ttype {
        TokenType::Let => {
            tokens.next(); // consume the Let
            match tokens.peek().unwrap().ttype {
                TokenType::Global => global_declaration(tokens),
                TokenType::Identifier(_) => variable_declaration(tokens),
                ref token @ _ => {
                    Err(ParseError {
                        token: (*tokens.peek().unwrap()).clone(),
                        message: format!("Unexpected {:?}, expected identifier or 'global'.", token),
                    })
                }
            }
        }
        _ => statement(tokens),
    }
}

fn global_declaration<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Statement, ParseError> {
    tokens.next(); // consume Global

    let identifier = match tokens.peek().unwrap().ttype {
        TokenType::Identifier(ref id) => {
            tokens.next();
            Ok(id.to_owned())
        }
        ref token @ _ => {
            Err(ParseError {
                token: (*tokens.peek().unwrap()).clone(),
                message: format!("Unexpected {:?}, expected Identifier.", token),
            })
        }
    }?;

    let initializer = match tokens.peek().unwrap().ttype {
        TokenType::Equal => {
            tokens.next(); // consume Equal
            Ok(Box::new(expression(tokens)?))
        }
        ref token @ _ => {
            Err(ParseError {
                token: (*tokens.peek().unwrap()).clone(),
                message: format!("Unexpected {:?}, expected '='.", token),
            })
        }
    }?;

    let next = tokens.next().unwrap();
    match next.ttype {
        TokenType::Semicolon => Ok(Statement::GlobalDeclaration(identifier, initializer)),
        _ => Err(ParseError {
            token: next.clone(),
            message: format!("Unexpected {:?}, expected semicolon ';'.", next.ttype),
        }),
    }
}

fn variable_declaration<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Statement, ParseError> {
    let identifier = match tokens.next().unwrap().ttype {
        TokenType::Identifier(ref id) => id.to_owned(),
        _ => panic!("expected identifier"),
    };

    let initializer = match tokens.peek().unwrap().ttype {
        TokenType::Semicolon => Ok(None),
        TokenType::Equal => {
            tokens.next();
            Ok(Some(Box::new(expression(tokens)?)))
        }
        ref token @ _ => {
            Err(ParseError {
                token: (*tokens.peek().unwrap()).clone(),
                message: format!("Unexpected {:?}, expected ';' or '='.", token),
            })
        }
    }?;

    let next = tokens.next().unwrap();
    match next.ttype {
        TokenType::Semicolon => Ok(Statement::VarDeclaration(identifier, initializer)),
        _ => Err(ParseError {
            token: next.clone(),
            message: format!("Unexpected {:?}, expected semicolon ';'.", next.ttype),
        }),
    }
}

fn statement<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Statement, ParseError> {
    match tokens.peek().unwrap().ttype {
        TokenType::If => {
            tokens.next();
            if_statement(tokens)
        }
        TokenType::For => {
            tokens.next().unwrap();
            for_statement(tokens)
        }
        TokenType::Print => {
            tokens.next();
            print_statement(tokens)
        }
        TokenType::LeftBrace => {
            block_statement(tokens)
        }
        _ => expression_statement(tokens),
    }
}

fn if_statement<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Statement, ParseError> {
    let condition = expression(tokens)?;
    let then = block_statement(tokens)?;
    let mut else_body = None;

    if tokens.peek().unwrap().ttype == TokenType::Else {
        tokens.next().unwrap(); // consume the Else
        else_body = Some(Box::new(block_statement(tokens)?));
    }

    Ok(Statement::If(Box::new(condition), Box::new(then), else_body))
}

fn for_statement<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Statement, ParseError> {
    let ids = expression(tokens)?;
    // consume In
    let next = tokens.next().unwrap();
    match next.ttype {
        TokenType::In => (),
        _ => return Err(ParseError {
            token: next.clone(),
            message: format!("Unexpected {:?}, expected 'in'.", next.ttype),
        }),
    }

    let iter = expression(tokens)?;
    let body = block_statement(tokens)?;

    let mut params = Vec::new();
    params.push(match ids {
            Expression::Variable(ref id) => id.to_string(),
            _ => panic!("Unexpected {:?}", ids),
    });
    Ok(Statement::For(params, Box::new(iter), Box::new(body)))
}

fn block_statement<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Statement, ParseError> {
    let mut statements = Vec::new();

    tokens.next().unwrap(); // consume LeftBrace
    while tokens.peek().unwrap().ttype != TokenType::RightBrace {
        statements.push(declaration(tokens)?);
    }

    tokens.next().unwrap(); // consume the RightBrace

    Ok(Statement::BlockStatement(statements))
}

fn print_statement<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Statement, ParseError> {
    let expr = expression(tokens)?;

    let next = tokens.next().unwrap();
    match next.ttype {
        TokenType::Semicolon => Ok(Statement::Print(Box::new(expr))),
        _ => Err(ParseError {
            token: next.clone(),
            message: format!("Unexpected {:?}, expected semicolon ';'.", next.ttype),
        }),
    }
}

fn expression_statement<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Statement, ParseError> {
    let expr = expression(tokens)?;

    match tokens.peek().unwrap().ttype {
        TokenType::Semicolon => {
            tokens.next().unwrap();
            Ok(Statement::Expression(Box::new(expr)))
        },
        ref token @ _ => {
            Err(ParseError {
                token: (*tokens.peek().unwrap()).clone(),
                message: format!("Unexpected {:?} in expression statement, expected semicolon ';'.", token),
            })
        },
    }
}

fn expression<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
    match tokens.peek().unwrap().ttype {
        TokenType::If => if_expression(tokens),
        _ => range(tokens)
    }
}

fn if_expression<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
    tokens.next().unwrap(); // consume the If

    let condition = expression(tokens)?;
    let then = block_expression(tokens)?;
    let mut else_body = None;


    if tokens.peek().unwrap().ttype == TokenType::Else {
        tokens.next().unwrap(); // consume the Else
        else_body = Some(Box::new(block_expression(tokens)?));
    }

    Ok(Expression::If(Box::new(condition), Box::new(then), else_body))
}

fn range<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
    let mut left = equality(tokens)?;

    while tokens.peek().unwrap().ttype == TokenType::DotDot {
        // ranges (1 .. 10) should desugar to blocks that return iterator functions
        // in future, I probably want Iterator traits or something

        // 1 .. 10 = { let i = 1; -> { i += 1; if i > 10 { nil } else { i }}}

        tokens.next().unwrap(); // consume the DotDot
        let right = equality(tokens)?;

        // create block expression to hold the counter variable
        let block = Expression::BlockExpression(
            vec![
                // let i = 1;
                Statement::VarDeclaration("i".to_owned(),
                    Some(Box::new(left))),
            ],
            // -> {
            Box::new(Expression::FunctionDeclaration(Vec::new(),
                Box::new(Expression::BlockExpression(
                    // i += 1;
                    vec![Statement::Expression(Box::new(
                        Expression::Binary {
                            operator: Token::dummy(TokenType::PlusEqual),
                            left: Box::new(Expression::Variable("i".to_owned())),
                            right: Box::new(Expression::Literal(Literal::Number(1.0))),
                        }
                    ))],
                    // if
                    Box::new(
                        Expression::If(
                            // i > 10
                            Box::new(Expression::Binary {
                                operator: Token::dummy(TokenType::Greater),
                                left: Box::new(Expression::Variable("i".to_owned())),
                                right: Box::new(right),
                            }),
                            // nil
                            Box::new(Expression::Literal(Literal::Nil)),
                            // i
                            Some(Box::new(Expression::Variable("i".to_owned()))),
                        )
                    )
                )
            ))),
        );

        left = block;
    }

    Ok(left)
}

fn equality<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
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

fn comparison<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
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

fn addition<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
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
) -> Result<Expression, ParseError> {
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

fn unary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
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

    call(tokens)
}

//   ____
//  (.   \
//    \  |   
//     \ |___(\--/)
//   __/    (  . . )
//  "'._.    '-.O.'
//       '-.  \ "|\
//          '.,,/'.,,mrf


fn call<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
    let mut expr = primary(tokens)?;

    loop {
        if tokens.peek().unwrap().ttype == TokenType::LeftParen {
            tokens.next().unwrap(); // consume LeftParen
            expr = finish_call(expr, tokens)?;
        } else {
            break;
        }
    }

    Ok(expr)
}

fn finish_call<'a>(
    callee: Expression,
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Expression, ParseError> {
    let mut arguments = Vec::new();

    if tokens.peek().unwrap().ttype != TokenType::RightParen {
        loop {
            arguments.push(expression(tokens)?);

            if tokens.peek().unwrap().ttype != TokenType::Comma {
                break;
            }
        }
    }

    let next = tokens.next().unwrap();
    match next.ttype {
        TokenType::RightParen => Ok(Expression::FunctionCall(Box::new(callee), arguments)),
        _ => Err(ParseError {
            token: next.clone(),
            message: format!(
                "Unexpected {:?}, expected closing parenthesis ')'.",
                next.ttype
            ),
        }),
    }
}

fn primary<'a>(tokens: &mut Peekable<Iter<'a, Token>>) -> Result<Expression, ParseError> {
    match tokens.peek().unwrap().ttype {
        TokenType::Identifier(ref id) => {
            tokens.next().unwrap(); // consume the token
            Ok(Expression::Variable(id.clone()))
        },
        TokenType::Number(val) => {
            tokens.next().unwrap();
            Ok(Expression::Literal(Literal::Number(val)))
        },
        TokenType::String(ref val) => {
            tokens.next().unwrap();
            Ok(Expression::Literal(Literal::String(val.clone())))
        },
        TokenType::False => {
            tokens.next().unwrap();
            Ok(Expression::Literal(Literal::Boolean(false)))
        },
        TokenType::True => {
            tokens.next().unwrap();
            Ok(Expression::Literal(Literal::Boolean(true)))
        },
        TokenType::Nil => {
            tokens.next().unwrap();
            Ok(Expression::Literal(Literal::Nil))
        },
        TokenType::LeftParen => {
            // LeftParen can start either a grouped expression or a arugment list
            // for a function declaration
            tokens.next().unwrap(); // consume the LeftParen

            // early return for ()
            if tokens.peek().unwrap().ttype == TokenType::RightParen {
                return Ok(Expression::Literal(Literal::Nil));
            }

            // if next token is comma, it is a tuple
            // if next token is right paren, it is a grouping
            // anything else is a parse error
            let expr = expression(tokens)?;

            let ret = match tokens.peek().unwrap().ttype {
                TokenType::RightParen => {
                    tokens.next().unwrap(); // consume the RightParen
                    if tokens.peek().unwrap().ttype == TokenType::Arrow {
                        let expressions = vec![expr];

                        let params = expressions.iter().map(|e| {
                            match e {
                                Expression::Variable(ref id) => id.to_owned(),
                                _ => panic!("Unexpected {:?}", e),
                            }
                        }).collect();

                        // consume the arrow
                        tokens.next().unwrap();
                        let body = block_expression(tokens)?;
                        Ok(Expression::FunctionDeclaration(params, Box::new(body)))
                    } else {
                        Ok(Expression::Grouping(Box::new(expr)))
                    }
                },
                TokenType::Comma => {
                    tokens.next().unwrap();
                    let mut expressions = Vec::new();
                    expressions.push(expr);

                    while tokens.peek().unwrap().ttype != TokenType::RightParen {
                        expressions.push(expression(tokens)?);
                    }

                    // consume the RightParen
                    tokens.next().unwrap();

                    match tokens.peek().unwrap().ttype {
                        TokenType::RightParen => {
                            // TODO: figure out tuples
                            unimplemented!();
                        },
                        TokenType::Arrow => {
                            // Arrow means FunctionDeclaration
                            // map expressions to param list
                            let params = expressions.iter().map(|e| {
                                match e {
                                    Expression::Variable(ref id) => id.to_string(),
                                    _ => panic!("Unexpected {:?}", e),
                                }
                            }).collect();

                            // consume the arrow
                            tokens.next().unwrap();
                            let body = block_expression(tokens)?;
                            Ok(Expression::FunctionDeclaration(params, Box::new(body)))
                        },
                        ref token @ _ => Err(ParseError {
                            token: (*tokens.peek().unwrap()).clone(),
                            message: format!(
                                "Unexpected {:?}",
                                token
                            ),
                        }),
                    }
                },
                ref token @ _ => Err(ParseError {
                    token: (*tokens.peek().unwrap()).clone(),
                    message: format!(
                        "Unexpected {:?}, expected closing parenthesis ')' or comma ','.",
                        token
                    ),
                }),
            };

            ret
        }
        TokenType::Arrow => {
            tokens.next().unwrap();
            match tokens.peek().unwrap().ttype {
                TokenType::LeftBrace => (),
                _ => {
                    let next = tokens.next().unwrap();
                    return Err(ParseError {
                        token: next.clone(),
                        message: format!(
                            "Unexpected {:?}, expected closing parenthesis ')'.",
                            next.ttype
                        ),
                    })
                }
            };
            let body = block_expression(tokens)?;

            Ok(Expression::FunctionDeclaration(Vec::new(), Box::new(body)))
        }
        TokenType::LeftBrace => block_expression(tokens),
        _ => {
            let token = tokens.next().unwrap();
            Err(ParseError {
                token: token.clone(),
                message: format!("Unexpected {:?}.", token.ttype),
            })
        },
    }
}

fn block_expression<'a>(
    tokens: &mut Peekable<Iter<'a, Token>>,
) -> Result<Expression, ParseError> {
    tokens.next().unwrap(); // consume the opening brace

    let mut statements = Vec::new();
    let mut ret = None;

    while tokens.peek().unwrap().ttype != TokenType::RightBrace {
        // clone the token iterator, so we don't consume any tokens if there is
        // a parse error
        let mut try_tokens = tokens.clone();
        match declaration(&mut try_tokens) {
            Ok(stmt) => statements.push(stmt),
            Err(p_err) => {
                ret = Some(expression(tokens).map_err(|_e| { p_err })?);
                break;
            }
        }

        while tokens.len() > try_tokens.len() {
            tokens.next().unwrap();
        }
    }

    let next = tokens.next().unwrap(); // consume the closing brace
    match next.ttype {
        TokenType::RightBrace => (),
        _ => return Err(ParseError {
            token: next.clone(),
            message: format!("Unexpected {:?}, expected closing brace '}}'.", next.ttype),
        }),
    }

    Ok(Expression::BlockExpression(
        statements,
        Box::new(match ret {
            Some(expr) => expr,
            None => Expression::Literal(Literal::Nil)
        }),
    ))
}
