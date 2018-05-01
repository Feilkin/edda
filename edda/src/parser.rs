//! Parser
//! parses and stuff

use std::mem;

use token::Token;
use ast::{Statement, Expression, Literal};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Vec<Statement> {
    	let mut statements = Vec::new();
    	while !self.is_at_end() {
    		statements.push(self.declaration());
    	}

    	statements
    }

    fn declaration(&mut self) -> Statement {
    	if self.check(Token::Let) {
    		self.advance();
    		return self.var_declaration();
    	}

    	self.statement()
    }

    fn var_declaration(&mut self) -> Statement {
    	assert!(self.check(Token::Identifier("".to_owned())));
    	let name = match self.advance() {
    		Token::Identifier(ref id) => id.to_owned(),
    		_ => panic!("wtf is happening")
    	};

    	let mut initializer = None;

    	if self.check(Token::Equal) {
    		self.advance();

    		initializer = Some(Box::new(self.expression()));
    	}

    	assert_eq!(self.advance(), Token::Semicolon);

    	Statement::VarDeclaration(name.clone(), initializer)
    }

    fn statement(&mut self) -> Statement {
    	if self.check(Token::Print) {
    		self.advance();
    		return self.print_statement();
    	}

    	self.expression_statement()
    }

    fn print_statement(&mut self) -> Statement {
    	let expr = self.expression();
    	assert_eq!(self.advance(), Token::Semicolon);

    	Statement::Print(Box::new(expr))
    }

    fn expression_statement(&mut self) -> Statement {
    	let expr = self.expression();
    	assert_eq!(self.advance(), Token::Semicolon);

    	Statement::Expression(Box::new(expr))
    }

    fn expression(&mut self) -> Expression {
        self.equality()
    }

    fn equality(&mut self) -> Expression {
        let mut left = self.comparison();

        while self.check(Token::BangEqual) || self.check(Token::EqualEqual) {
            let operator = self.advance();
            let right = self.comparison();
            left = Expression::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn comparison(&mut self) -> Expression {
        let mut left = self.addition();

        while self.check(Token::Greater) || self.check(Token::GreaterEqual)
            || self.check(Token::Less) || self.check(Token::LessEqual)
        {
            let operator = self.advance();
            let right = self.addition();
            left = Expression::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn addition(&mut self) -> Expression {
        let mut left = self.multiplication();

        while self.check(Token::Plus) || self.check(Token::Minus) {
            let operator = self.advance();
            let right = self.multiplication();
            left = Expression::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn multiplication(&mut self) -> Expression {
        let mut left = self.unary();

        while self.check(Token::Slash) || self.check(Token::Star) {
            let operator = self.advance();
            let right = self.unary();
            left = Expression::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn unary(&mut self) -> Expression {
        if self.check(Token::Bang) || self.check(Token::Minus) {
            let operator = self.advance();
            let expr = self.unary();
            return Expression::Unary {
                operator,
                expr: Box::new(expr),
            };
        }

        self.primary()
    }

    fn primary(&mut self) -> Expression {
        let token = self.advance();

        match token {
            Token::Number(val) => Expression::Literal(Literal::Number(val)),
            Token::String(ref val) => Expression::Literal(Literal::String(val.clone())),
            Token::False => Expression::Literal(Literal::Boolean(false)),
            Token::True => Expression::Literal(Literal::Boolean(true)),
            Token::Nil => Expression::Literal(Literal::Nil),
            Token::LeftParen => {
                let expr = self.expression();
                assert_eq!(self.advance(), Token::RightParen);

                Expression::Grouping(Box::new(expr))
            }
            Token::Identifier(ref name) => Expression::Variable(name.clone()),
            _ => panic!("Failed to parse, unexpected {:?}", token),
        }
    }

    fn check(&self, token: Token) -> bool {
        if self.is_at_end() {
            return false;
        }
        mem::discriminant(&self.peek()) == mem::discriminant(&token)
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek() == Token::EoF
    }

    fn peek(&self) -> Token {
        self.tokens.get(self.current).unwrap().clone()
    }

    fn previous(&self) -> Token {
        self.tokens.get(self.current - 1).unwrap().clone()
    }
}
