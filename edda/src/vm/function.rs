//! Static functions :)

use crate::typer::Type;
use crate::Chunk;

pub struct Function {
    name: String,
    chunk: Chunk,
    signature: (Vec<Type>, Type),
}
