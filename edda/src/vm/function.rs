//! Static functions :)

use crate::token::Token;
use crate::typer::Type;
use crate::{Chunk, Expression};

/// Represents function definition while source is being compiled.
/// Keeps track of each call to this function in the code, so the calls can be replaced with
/// actual address to the function.
pub struct FunctionDeclaration<'s> {
    pub name: Token<'s>,
    pub signature: (Vec<Type>, Type),
    pub body: Box<Expression<'s>>,
    pub references: Vec<usize>,
}
