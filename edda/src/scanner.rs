//! it scans text

// TODO: on-demand scanning
use crate::token::{Token, TokenType};

type ScanResult<'s> = Result<Vec<Token<'s>>, ScanError>;

#[derive(Debug, Eq, PartialEq)]
pub struct ScanError {
    pub offset: usize,
    pub c: char,
}

macro_rules! trie {
    // single key
    (@key $source:expr, $val:expr, $key:literal,) => {
        $val == $key
    };

    // list of keys
    (@key $source:expr, $val:expr, $first:literal, $($tail:literal,)+) => {
        trie!(@key $source, $val, $first,) && {
            let next = $source.next();
            match next {
                Some((_, c)) => trie!(@key $source, c, $($tail,)+),
                None => false,
            }
        }
    };

    // branch value
    (@value $source:ident, [$($rules:tt)+]) => {
        match match $source.next() {
            Some((_, c)) => trie!($source, c, [$($rules)+]),
            None => None
        } {
            Some(value) => Some((value, $source)),
            None => None,
        }
    };

    // leaf value
    (@value $source:ident, $value:expr) => {
        Some(($value, $source))
    };

    // Entry point with branch
    ($source:ident, $val:expr, [
        $($keys:literal)+ => $value:tt,
        $($tail:tt)*
    ]) => {{
        let mut source = $source.clone(); // prevent mutation
        if trie!(@key source, $val, $($keys,)*) {
            match trie!(@value source, $value) {
                Some((value, source)) => {
                    $source = source;
                    Some(value)
                },
                None => None
            }
        } else {
            trie!($source, $val, [$($tail)*])
        }
    }};

    // exit
    ($source:expr, $val:expr, []) => {
        None
    }
}

macro_rules! match_tokens {
    (match ($offset:ident, $char:ident) $iter:ident {
        // match bare arms
        simple [$($s_type:path = $s_token:expr)*]
        double [$($d_type1:path, $d_type2:path = $d_token1:expr, $d_token2:expr)*]
        $(trie [$trie_root:pat => $trie_args:tt])*
        $(ignore [$($ignr:expr)+])?
    }) => {{

        match $char {
            $($s_token => Ok(Some(($s_type, 1))),)*
            $($d_token1 => {
                if let Some((_, next_char)) = $iter.peek() {
                    if next_char == &$d_token2 {
                        $iter.next();
                        Ok(Some(($d_type2, 2)))
                    } else {
                        Ok(Some(($d_type1, 1)))
                    }
                } else {
                    Ok(Some(($d_type1, 1)))
                }
            },)*

            $(val @ $trie_root => match trie!($iter, val, $trie_args) {
                Some((t_type, len)) => Ok(Some((t_type, len))),
                None => Err(ScanError {
                    $offset,
                    c: val
                })
            },)*

            $(
                $($ignr => Ok(None),)+
            )?
            _ => Err(ScanError {
                $offset,
                $char
            }),
        }
    }};
}

pub fn scan(source: &str) -> ScanResult {
    // TODO: get rid of these mutable locals
    let mut tokens = Vec::new();
    let mut chars_and_offsets = source.char_indices().peekable();

    while let Some((offset, c)) = chars_and_offsets.next() {
        use TokenType::*;
        if let Some((t_type, length)) = match_tokens! {
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
                ]

                trie [
                    'a'..='z' => [
                        'a' 'n' 'd' => (And, 3),
                        'e' 'l' 's' 'e' => (Else, 4),
                        'f' => [
                            // TODO: functions
                            'o' 'r' => (For, 3),
                            'a' 'l' 's' 'e' => (False, 5),
                        ],
                        'i' => [
                            'f' => (If, 2),
                            'n' => (In, 2),
                        ],
                        'o' 'r' => (Or, 2),
                        'p' 'r' 'i' 'n' 't' => (Print, 5),
                        't' 'r' 'u' 'e' => (True, 4),
                    ]
                ]

                ignore [
                    ' '
                    '\t'
                    '\n'
                    '\r'
                ]
            }
        }? {
            tokens.push(Token {
                t_type,
                offset,
                text: &source[offset..offset + length],
            })
        }
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

    #[test]
    fn test_skip_whitespace() {
        assert_eq!(
            scan("( )"),
            Ok(vec![
                Token {
                    t_type: TokenType::LeftParen,
                    offset: 0,
                    text: "("
                },
                Token {
                    t_type: TokenType::RightParen,
                    offset: 2,
                    text: ")"
                },
            ])
        );
    }

    #[test]
    fn test_keyword() {
        assert_eq!(
            scan("and"),
            Ok(vec![
                Token {
                    t_type: TokenType::And,
                    offset: 0,
                    text: "and"
                }
            ])
        );
    }

    #[test]
    fn test_nested_trie_keyword() {
        assert_eq!(
            scan("false for"),
            Ok(vec![
                Token {
                    t_type: TokenType::False,
                    offset: 0,
                    text: "false"
                },
                Token {
                    t_type: TokenType::For,
                    offset: 6,
                    text: "for"
                },
            ])
        )
    }
}
