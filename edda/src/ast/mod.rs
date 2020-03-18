//! Syntax tree nodes

use crate::ast::expressions::Binary;
use crate::parser::{semicolon, Parsable, ParseResult};
use crate::token::Token;
use std::any::Any;
use std::fmt::{Debug, Display, Error, Formatter};

mod expressions;

// TODO: do we actually need these?
pub trait Statement: Debug + Display {}
