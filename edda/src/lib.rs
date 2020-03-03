//! The new and improved edda
//!
//! use at your own risk

#[macro_use]
mod parser;

mod ast;
mod scanner;
mod token;

// exports
pub use scanner::scan;
