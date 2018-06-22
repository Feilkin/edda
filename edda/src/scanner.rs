//! Lexical Scanner
//! Scans for lexes and stuff

use token::{Token, TokenType, KEYWORDS};

#[derive(Debug)]
pub struct ScanError {
    start: usize,
    end: usize,
    message: String,
}

pub fn scan_tokens(source: &str) -> Result<Vec<Token>, ScanError> {
    let mut chars = source.chars().peekable();
    let mut tokens = Vec::new();
    let mut cur_pos = 0;

    while let Some(c) = chars.next() {
        let length;
        {
            let mut push_token = |tt, length| {
                tokens.push(Token {
                    ttype: tt,
                    start: cur_pos,
                    end: cur_pos + length,
                });

                Ok(length)
            };
            length = match c {
                '(' => push_token(TokenType::LeftParen, 1),
                ')' => push_token(TokenType::RightParen, 1),
                '{' => push_token(TokenType::LeftBrace, 1),
                '}' => push_token(TokenType::RightBrace, 1),
                '.' => {
                    if chars.peek() == Some(&'.') {
                        chars.next();
                        push_token(TokenType::DotDot, 2)
                    } else {
                        push_token(TokenType::Dot, 1)
                    }
                }
                ',' => push_token(TokenType::Comma, 1),
                ';' => push_token(TokenType::Semicolon, 1),
                '+' => push_token(TokenType::Plus, 1),
                '*' => push_token(TokenType::Star, 1),
                '^' => push_token(TokenType::Caret, 1),
                '-' => {
                    if chars.peek() == Some(&'>') {
                        chars.next();
                        push_token(TokenType::Arrow, 2)
                    } else {
                        push_token(TokenType::Minus, 1)
                    }
                }
                '!' => {
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        push_token(TokenType::BangEqual, 2)
                    } else {
                        push_token(TokenType::Bang, 1)
                    }
                }
                '=' => {
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        push_token(TokenType::EqualEqual, 2)
                    } else {
                        push_token(TokenType::Equal, 1)
                    }
                }
                '<' => {
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        push_token(TokenType::LessEqual, 2)
                    } else {
                        push_token(TokenType::Less, 1)
                    }
                }
                '>' => {
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        push_token(TokenType::GreaterEqual, 2)
                    } else {
                        push_token(TokenType::Greater, 1)
                    }
                }
                '/' => {
                    let mut length = 0;

                    if chars.peek() == Some(&'/') {
                        while let Some(cc) = chars.next() {
                            length += 1;

                            if cc == '\n' {
                                break;
                            }
                        }
                        Ok(length)
                    } else {
                        push_token(TokenType::Slash, 1)
                    }
                }
                ' ' | '\r' | '\t' | '\n' => Ok(1), // skip whitespace

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
                    push_token(
                        TokenType::Number(char_buffer.iter().collect::<String>().parse().unwrap()),
                        char_buffer.len(),
                    )
                }
                '"' => {
                    // String literal
                    let mut char_buffer = Vec::new();

                    while let Some(cc) = chars.next() {
                        if cc == '"' {
                            break;
                        } else {
                            char_buffer.push(cc.clone());
                        }
                    }

                    push_token(
                        TokenType::String(char_buffer.iter().collect()),
                        char_buffer.len() + 2,
                    )
                }

                'a'...'z' | 'A'...'Z' | '_' => {
                    // Identifier
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

                    let identifier = char_buffer.iter().collect::<String>();

                    if let Some(ref t) = KEYWORDS.get(identifier.as_str()) {
                        push_token((*t).clone(), char_buffer.len())
                    } else {
                        push_token(TokenType::Identifier(identifier), char_buffer.len())
                    }
                }

                // TODO: better error handling
                _ => Err(ScanError {
                    start: cur_pos,
                    end: cur_pos + 1,
                    message: format!("Unexpected {}.", c),
                }),
            }?;
        }

        cur_pos += length;
    }

    tokens.push(Token {
        ttype: TokenType::EoF,
        start: cur_pos,
        end: cur_pos + 1,
    });
    Ok(tokens)
}

// TODO: rewrite tests
/*
#[cfg(test)]
mod tests {
    use super::scan_tokens;
    use super::TokenType::*;

    #[test]
    fn scan_numbers() {
        let tokens = scan_tokens("1234");

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
*/
