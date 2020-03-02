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
    // leaf nodes
    // single key
    (@key_lhs $source:expr, $val:expr, $key:literal,) => {
        $val == $key
    };

    (@key_leaf_rhs $source:expr, $val:expr,) => {{
        if let Some(next) = $source.peek() {
            match next.1 {
                'a' ..= 'z' | '0' ..= '9' | '_' => false,
                _ => true
            }
        } else {
            true
        }
    }};

    (@key_leaf_rhs $source:expr, $val:expr, $($tail:literal,)+) => {
        trie!(@key_leaf $source, $val, $($tail,)*)
    };

    // list of keys
    (@key_leaf $source:expr, $val:expr, $first:literal, $($tail:literal,)+) => {
        trie!(@key_lhs $source, $val, $first,) && {
            let next = $source.next();
            match next {
                Some((_, c)) => trie!(@key_leaf_rhs $source, c, $($tail,)+),
                None => false,
            }
        }
    };

    // last key
    (@key_leaf $source:expr, $val:expr, $first:literal,) => {
        trie!(@key_lhs $source, $val, $first,) && trie!(@key_leaf_rhs $source, $val,)
    };

    // branch nodes
    (@key_branch $source:expr, $val:expr, $first:literal,) => {
        trie!(@key_lhs $source, $val, $first,)
    };

    (@key_branch $source:expr, $val:expr, $first:literal, $($tail:literal,)+) => {
        trie!(@key_lhs $source, $val, $first,) && {
            let next = $source.next();
            match next {
                Some((_, c)) => trie!(@key_lhs $source, c, $($tail,)+),
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
        $($keys:literal)+ => [$($value:tt)+],
        $($tail:tt)*
    ]) => {{
        let mut source = $source.clone(); // prevent mutation
        if trie!(@key_branch source, $val, $($keys,)+) {
            match trie!(@value source, [$($value)*]) {
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

    // Entry point with leaf
    ($source:ident, $val:expr, [
        $($keys:literal)+ => $value:expr,
        $($tail:tt)*
    ]) => {{
        let mut source = $source.clone(); // prevent mutation
        if trie!(@key_leaf source, $val, $($keys,)+) {
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
        $(trie_or [$trie_root_d:pat => $trie_args_d:tt] -> $default:expr;)*
        $(trie [$trie_root:pat => $trie_args:tt])*
        $(custom $custom_rule:pat => ($custom_char:ident, $custom_source:ident) $custom_body:tt)*
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

            $(val @ $trie_root_d => match trie!($iter, val, $trie_args_d) {
                Some((t_type, len)) => Ok(Some((t_type, len))),
                None => {
                    let mut source = $iter.clone();
                    match $default($char, &mut source) {
                        Some((t_type, len)) => {
                            $iter = source;
                            Ok(Some((t_type, len)))
                        },
                        None => Err(ScanError {
                            $offset,
                            c: val
                        })
                    }
                }
            },)*

            $(val @ $trie_root => match trie!($iter, val, $trie_args) {
                Some((t_type, len)) => Ok(Some((t_type, len))),
                None => Err(ScanError {
                    $offset,
                    c: val
                })
            },)*

            $(val @ $custom_rule => {
                let $custom_char = val;
                let mut $custom_source = $iter.clone();

                match $custom_body {
                    Some((t_type, len)) => {
                        $iter = $custom_source;

                        Ok(Some((t_type, len)))
                    },
                    None => Err(ScanError {
                        $offset,
                        c: val
                    })
                }
            })*

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
        use std::iter::Peekable;
        use std::str::CharIndices;
        use TokenType::*;

        if let Some((t_type, length)) = match_tokens! {
            match (offset, c) chars_and_offsets {
                simple [
                    LeftParen     = '('
                    RightParen    = ')'
                    LeftBrace     = '{'
                    RightBrace    = '}'
                    Comma         = ','
                    Semicolon     = ';'
                ]

                double [
                    Dot, DotDot       = '.', '.'
                    Bang, BangEqual   = '!', '='
                    Plus, PlusEqual   = '+', '='
                    Star, StarEqual   = '*', '='
                ]

                trie_or [
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
                        'l' 'e' 't' => (Let, 3),
                        'o' 'r' => (Or, 2),
                        'p' 'r' 'i' 'n' 't' => (Print, 5),
                        't' 'r' 'u' 'e' => (True, 4),
                    ]
                ] -> (|c, source: &mut Peekable<CharIndices>| {
                    // TODO: make identifier here
                    let mut len = 1;

                    while match source.peek() {
                        Some((_, c)) => match c {
                            // valid identifier characters
                            'a'..='z' |
                            'A'..='Z' |
                            '0'..='9' |
                            '_' => true,
                            // anything else ends the identifier
                            _ => false,
                        },
                        None => false
                    } { source.next(); len += 1; }

                    Some((Identifier, len))
                });

                trie_or [
                    '=' => [
                        '=' => [
                            '=' => (EqualEqual, 2),
                            '>' => (FatArrow, 2),
                        ],
                    ]
                ] -> (|c, _source| {
                    Some((Equal, 1))
                });

                custom '0' ..= '9' => (c, source) {
                    let mut len = 1;

                    while match source.peek() {
                        Some((_, cc)) => match cc {
                            '0' ..= '9' => true,
                            _ => false
                        },
                        None => false,
                    } {
                        source.next();
                        len += 1;
                    }

                    Some((Integer, len))
                }

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

    #[test]
    fn test_simple_identifier() {
        assert_eq!(
            scan("let juttu;"),
            Ok(vec![
                Token {
                    t_type: TokenType::Let,
                    offset: 0,
                    text: "let"
                },
                Token {
                    t_type: TokenType::Identifier,
                    offset: 4,
                    text: "juttu"
                },
                Token {
                    t_type: TokenType::Semicolon,
                    offset: 9,
                    text: ";"
                },
            ])
        )
    }

    #[test]
    fn test_keyword_like_identifier() {
        assert_eq!(
            scan("let fat;"),
            Ok(vec![
                Token {
                    t_type: TokenType::Let,
                    offset: 0,
                    text: "let"
                },
                Token {
                    t_type: TokenType::Identifier,
                    offset: 4,
                    text: "fat"
                },
                Token {
                    t_type: TokenType::Semicolon,
                    offset: 7,
                    text: ";"
                },
            ])
        )
    }

    #[test]
    fn test_keyword_break() {
        assert_eq!(
            scan("let falsey;"),
            Ok(vec![
                Token {
                    t_type: TokenType::Let,
                    offset: 0,
                    text: "let"
                },
                Token {
                    t_type: TokenType::Identifier,
                    offset: 4,
                    text: "falsey"
                },
                Token {
                    t_type: TokenType::Semicolon,
                    offset: 10,
                    text: ";"
                },
            ])
        )
    }

    #[test]
    fn test_integer() {
        assert_eq!(
            scan("let a = 1337;"),
            Ok(vec![
                Token {
                    t_type: TokenType::Let,
                    offset: 0,
                    text: "let"
                },
                Token {
                    t_type: TokenType::Identifier,
                    offset: 4,
                    text: "a"
                },
                Token {
                    t_type: TokenType::Equal,
                    offset: 6,
                    text: "="
                },
                Token {
                    t_type: TokenType::Integer,
                    offset: 8,
                    text: "1337",
                },
                Token {
                    t_type: TokenType::Semicolon,
                    offset: 12,
                    text: ";"
                },
            ])
        )
    }
}
