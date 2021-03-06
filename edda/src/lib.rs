//! The new and improved edda
//!
//! use at your own risk

#[macro_use]
extern crate lazy_static;

#[macro_use]
pub mod parser;

mod ast;
mod scanner;
mod token;
mod typer;
mod vm;

// exports
pub use ast::expressions::Expression;
pub use scanner::scan;
pub use vm::{
    bytecode::{Chunk, OpCode},
    compiler::Compiler,
    Vm, VmState,
};

// error type??
use thiserror::Error;

use crate::parser::ParseError;
use crate::scanner::ScanError;
use crate::vm::compiler::CompilerError;
use std::fmt::{Display, Formatter};

#[derive(Error, Debug)]
pub enum Error {
    ScanError(ScanError),
    CompilerError(CompilerError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Edda error: ")?;

        match self {
            Error::ScanError(err) => write!(f, "{:?}", err),
            Error::CompilerError(err) => write!(f, "{:?}", err),
        }?;

        writeln!(f)
    }
}
