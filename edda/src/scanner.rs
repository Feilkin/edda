//! Lexical Scanner
//! Scans for lexes and stuff

use token::{Token, KEYWORDS};

#[derive(Debug)]
pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    line: u32,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source: source,
            tokens: Vec::new(),
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Vec<Token> {
        let mut chars = self.source.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                '(' => self.tokens.push(Token::LeftParen),
                ')' => self.tokens.push(Token::RightParen),
                '{' => self.tokens.push(Token::LeftBrace),
                '}' => self.tokens.push(Token::RightBrace),
                '.' => {
                    if chars.peek() == Some(&'.') {
                        self.tokens.push(Token::DotDot);
                        chars.next();
                    } else {
                        self.tokens.push(Token::Dot)
                    }
                }
                ',' => self.tokens.push(Token::Comma),
                ';' => self.tokens.push(Token::Semicolon),
                '+' => self.tokens.push(Token::Plus),
                '*' => self.tokens.push(Token::Star),
                '^' => self.tokens.push(Token::Caret),
                '-' => {
                    if chars.peek() == Some(&'>') {
                        self.tokens.push(Token::Arrow);
                        chars.next();
                    } else {
                        self.tokens.push(Token::Minus);
                    }
                }
                '!' => {
                    if chars.peek() == Some(&'=') {
                        self.tokens.push(Token::BangEqual);
                        chars.next(); // skip the peeked char as it is part of the token
                    } else {
                        self.tokens.push(Token::Bang);
                    }
                }
                '=' => {
                    if chars.peek() == Some(&'=') {
                        self.tokens.push(Token::EqualEqual);
                        chars.next();
                    } else {
                        self.tokens.push(Token::Equal);
                    }
                }
                '<' => {
                    if chars.peek() == Some(&'=') {
                        self.tokens.push(Token::LessEqual);
                        chars.next();
                    } else {
                        self.tokens.push(Token::Less);
                    }
                }
                '>' => {
                    if chars.peek() == Some(&'=') {
                        self.tokens.push(Token::GreaterEqual);
                        chars.next();
                    } else {
                        self.tokens.push(Token::Greater);
                    }
                }
                '/' => {
                    if chars.peek() == Some(&'/') {
                        while let Some(cc) = chars.next() {
                            if cc == '\n' {
                                self.line += 1;
                                break;
                            }
                        }
                    } else {
                        self.tokens.push(Token::Slash);
                    }
                }
                ' ' | '\r' | '\t' => {}
                '\n' => self.line += 1,

                '0'...'9' => {
                    // Number literal
                    let mut char_buffer = Vec::new();

                    char_buffer.push(c);
                    while let Some(&cc) = chars.peek() {
                        if cc.is_digit(10) {
                            char_buffer.push(cc.clone());
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    if chars.peek() == Some(&'.') {
                        char_buffer.push('.');
                        chars.next();

                        while let Some(&cc) = chars.peek() {
                            if cc.is_digit(10) {
                                char_buffer.push(cc.clone());
                                chars.next();
                            } else {
                                break;
                            }
                        }
                    }

                    self.tokens.push(Token::Number(
                        char_buffer.into_iter().collect::<String>().parse().unwrap(),
                    ));
                },
                '"' => { // String literal
                	let mut char_buffer = Vec::new();

                	while let Some(cc) = chars.next() {
                		if cc == '"' {
                			break;
                		} else {
                			char_buffer.push(cc.clone());
                		}
                	}

                	self.tokens.push(Token::String(char_buffer.into_iter().collect()));
                }

                'a'...'z' | 'A'...'Z' | '_' => { // Identifier
                    let mut char_buffer = Vec::new();
                    char_buffer.push(c);

                    while let Some(&cc) = chars.peek() {
                        if cc.is_alphanumeric() || cc == '_' {
                            char_buffer.push(cc.clone());
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    let identifier = char_buffer.into_iter().collect::<String>();

                    if let Some(ref t) = KEYWORDS.get(identifier.as_str()) {
                        self.tokens.push((*t).clone());
                    } else {
                        self.tokens.push(Token::Identifier(identifier));
                    }
                }

                _ => panic!("Unable to parse {:?}, at line {}", c, self.line),
            }
        }

        self.tokens.push(Token::EoF);
        self.tokens
    }
}

#[cfg(test)]
mod tests {
    use super::Scanner;
    use super::Token::*;

    #[test]
    fn scan_numbers() {
        let scanner = Scanner::new("1234".to_owned());
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens, vec![Number(1234.), EoF]);
    }

    #[test]
    fn scan_decimals() {
        let scanner = Scanner::new(
            "1.337 0.9999 0.999999999 1 1.0 1.00000000000000
	    	1.00000000000001 0.99999999999999999999 // last one will round to 1.0"
                .to_owned(),
        );
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Number(1.337),
                Number(0.9999),
                Number(0.999999999),
                Number(1.0),
                Number(1.0),
                Number(1.0),
                Number(1.00000000000001),
                Number(1.0),
                EoF,
            ]
        );
    }

    #[test]
    fn scan_comment() {
        let scanner = Scanner::new("// only a comment".to_owned());
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens, vec![EoF]);
    }

    #[test]
    fn scan_multiple_lines_with_comments() {
        let scanner = Scanner::new(
            "1 // first line
		                            2 // second line
		                            3 // third line"
                .to_owned(),
        );
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens, vec![Number(1.), Number(2.), Number(3.), EoF]);
    }

    #[test]
    fn scan_math_with_comment() {
        let scanner = Scanner::new("2 + 2 = 4, - 1 = 3 // quick maths".to_owned());
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Number(2.),
                Plus,
                Number(2.),
                Equal,
                Number(4.),
                Comma,
                Minus,
                Number(1.),
                Equal,
                Number(3.),
                EoF,
            ]
        );
    }

    #[test]
    fn scan_parentheses() {
        let scanner = Scanner::new("1 + (3 * 4) ^ 2".to_owned());
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Number(1.),
                Plus,
                LeftParen,
                Number(3.),
                Star,
                Number(4.),
                RightParen,
                Caret,
                Number(2.),
                EoF,
            ]
        );
    }

    #[test]
    fn scan_keyword() {
        let scanner = Scanner::new("let add = (a, b) -> { a + b } // adds b to a".to_owned());
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Let,
                Identifier("add".to_owned()),
                Equal,
                LeftParen,
                Identifier("a".to_owned()),
                Comma,
                Identifier("b".to_owned()),
                RightParen,
                Arrow,
                LeftBrace,
                Identifier("a".to_owned()),
                Plus,
                Identifier("b".to_owned()),
                RightBrace,
                EoF,
            ]
        );
    }

    #[test]
    fn scan_loop() {
        let scanner = Scanner::new(
            "let prod = (a, b) -> {
	    	let acc = 0;
	    	for _ in 1 .. a {
	    		acc = acc + b
	    	}
	    }"
                .to_owned(),
        );
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Let,
                Identifier("prod".to_owned()),
                Equal,
                LeftParen,
                Identifier("a".to_owned()),
                Comma,
                Identifier("b".to_owned()),
                RightParen,
                Arrow,
                LeftBrace,
                Let,
                Identifier("acc".to_owned()),
                Equal,
                Number(0.),
                Semicolon,
                For,
                Identifier("_".to_owned()),
                In,
                Number(1.0),
                DotDot,
                Identifier("a".to_owned()),
                LeftBrace,
                Identifier("acc".to_owned()),
                Equal,
                Identifier("acc".to_owned()),
                Plus,
                Identifier("b".to_owned()),
                RightBrace,
                RightBrace,
                EoF,
            ]
        );
    }

    #[test]
    #[should_panic]
    fn scan_emoji() {
        let scanner = Scanner::new("let 😝 = 3 // no emoji allowed".to_owned());
        let _tokens = scanner.scan_tokens();
    }

    #[test]
    fn scan_string() {
        let scanner = Scanner::new("\"abcd\"".to_owned());
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens, vec![String("abcd".to_owned()), EoF]);
    }
}
