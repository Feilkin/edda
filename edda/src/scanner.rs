//! it scans text

// TODO: on-demand scanning

use crate::token::{Token, TokenType};

type ScanResult<'s> = Result<Vec<Token<'s>>, ScanError>;

#[derive(Debug, Eq, PartialEq)]
pub struct ScanError {
    pub offset: usize,
    pub c: char,
}

macro_rules! match_tokens {
    (match ($offset:ident, $char:ident) $iter:ident {
        simple [$($s_type:path = $s_token:expr)*]
        double [$($d_type1:path, $d_type2:path = $d_token1:expr, $d_token2:expr)*]
    }) => {
        match $char {
            $($s_token => Ok(($s_type, 1)),)*
            $($d_token1 => {
                if let Some((_, next_char)) = $iter.peek() {
                    if next_char == &$d_token2 {
                        $iter.next();
                        Ok(($d_type2, 2))
                    } else {
                        Ok(($d_type1, 1))
                    }
                } else {
                    Ok(($d_type1, 1))
                }
            },)*
            _ => Err(ScanError {
                $offset,
                $char
            }),
        }
    };
}

fn scan(source: &str) -> ScanResult {
    // TODO: get rid of these mutable locals
    let mut tokens = Vec::new();
    let mut chars_and_offsets = source.char_indices().peekable();

    while let Some((offset, c)) = chars_and_offsets.next() {
        use TokenType::*;
        let (t_type, length) = match_tokens! {
            match (offset, c) chars_and_offsets {
                simple [
                    LeftParen     = '('
                    RightParen    = ')'
                    LeftBrace     = '{'
                    RightBrace    = '}'
                    Comma         = ','
                ]

                double [
                    Dot, DotDot       = '.', '.'
                    Bang, BangEqual   = '!', '='
                    Plus, PlusEqual   = '+', '='
                    Star, StarEqual   = '*', '='
                    Equal, EqualEqual = '=', '='
                ]
            }
        }?;

        tokens.push(Token {
            t_type,
            offset,
            text: &source[offset..offset + length],
        })
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use crate::scanner::scan;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_parenthesis() {
        assert_eq!(
            scan("()"),
            Ok(vec![
                Token {
                    t_type: TokenType::LeftParen,
                    offset: 0,
                    text: "("
                },
                Token {
                    t_type: TokenType::RightParen,
                    offset: 1,
                    text: ")"
                },
            ])
        );
    }
}
