//! Static functions :)

use crate::token::Token;
use crate::typer::Type;
use crate::{Chunk, Expression};

/// Represents function definition while source is being compiled.
/// Keeps track of each call to this function in the code, so the calls can be replaced with
/// actual address to the function.
pub struct FunctionDeclaration<'s> {
    pub name: Token<'s>,
    pub signature: (Vec<(Token<'s>, Type)>, Type),
    // here, these 2 options are creating 4 possible states, when we only want 2 states (foward
    // declaration or offset)
    pub body: Option<Box<Expression<'s>>>,
    pub offset: Option<usize>,
    pub references: Vec<usize>,
}
