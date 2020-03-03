//! Syntax tree nodes

use crate::parser::{semicolon, Parsable, ParseResult};
use crate::token::Token;
use std::fmt::{Display, Error, Formatter, Debug};
use crate::ast::expressions::Binary;

mod expressions;

// TODO: do we actually need these?
pub trait Expression: Debug + Display {}
pub trait Statement: Debug + Display {}