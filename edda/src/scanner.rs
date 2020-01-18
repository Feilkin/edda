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
    // leaf
    (@value $source:expr, $t_type:path) => {{
        // TODO: advance the source iterator by val length
        Some($t_type)
    }};

    // branch
    (@value: $source:expr, [
        $($inner_key:literal => $value:tt,)+
    ]) => {{
        // advance the source iterator
        let val = val.next();
        match val {
            $(val @ $key => trie!(@inner $source, $value))+
            _ => None
        }
    }};

    (@key $source:expr, $key:literal,) => {
        $key
    };

    // parse keys
    (@key $source:expr, $first:literal, $second:literal, $($rest:literal,)*) => {
        $first if match $source.peek() {
            Some(c) if c == $second => {
                let source = $source.clone(); // prevent mutation
                source.next(); // advance the source iterator

                match source.peek() {
                    trie!(@key source, $($rest:literal,)*) => { true },
                    _ => { false }
                }
            },
            _ => false
        }
    };

    // Entry point
    ($source:expr, $val:expr, [
        $([$($keys:literal)+] => $value:tt,)+
    ]) => {{
        // clone the original iterator
        let source = $source.clone();

        match $val {
            $(trie!(@key source, $($keys,)+) => trie!(@value source, $value),)+

            // TODO: identifiers
            _ => None
        }
    }};
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

            val @ $($trie_root => match trie!($iter, val, $trie_args) {
                Some((t_type, len)) => Ok(Some((t_type, len))),
                None => panic!("syntax error")
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
                    Equal, EqualEqual = '=', '='
                ]

                trie [
                    'a'..='z' => [
                        ['a' 'n' 'd'] => And,
//                        "else" => Else,
//                        "f" => [
//                            // TODO: functions
//                            "or" => For,
//                            "alse" => False,
//                        ],
//                        "i" => [
//                            "f" => If,
//                            "n" => In,
//                        ],
//                        "or" => Or,
//                        "print" => Print,
//                        "true" => True,
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
}
