//! The new and improved edda
//!
//! use at your own risk

#[macro_use]
pub mod parser;

mod ast;
mod scanner;
mod token;
mod vm;

// exports
pub use scanner::scan;
pub use ast::expressions::Expression;
pub use vm::{Vm, bytecode::Chunk, compiler::compile};

// error type??
use thiserror::Error;

use crate::scanner::ScanError;
use crate::parser::ParseError;
use crate::vm::compiler::CompilerError;
use std::fmt::{Display, Formatter};

#[derive(Error, Debug)]
pub enum Error {
    ScanError(ScanError),
    CompilerError(CompilerError)
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Edda error: ")?;

        match self {
            Error::ScanError(err) => write!(f, "{:?}", err),
            Error::CompilerError(err) => write!(f, "{:?}", err)
        }?;

        writeln!(f)
    }
}