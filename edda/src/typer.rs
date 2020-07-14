//! Edda type system

use thiserror::Error;

use crate::Expression;
use std::fmt::{Display, Formatter};

#[derive(Error, Debug)]
#[error("Unexpected {err}, expected {expected}")]
pub struct TypeError {
    pub err: Type,
    pub expected: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    I32,
    Boolean,
    /// Function reference
    Function(Vec<Type>, Box<Type>),
    /// Pseudotype that is used when no concrete type is not yet needed
    Any,
}

impl Type {
    pub fn compile_time_size(&self) -> Option<usize> {
        match self {
            Type::I32 => Some(4),
            Type::Boolean => Some(1),
            Type::Function(..) => None,
            Type::Any => None,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let s = match self {
            Type::Boolean => "bool".to_owned(),
            Type::I32 => "i32".to_owned(),
            Type::Function(params, ret) => format!(
                "({}) -> {}",
                params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(", "),
                ret
            ),
            Type::Any => "<any>".to_owned(),
        };

        write!(f, "{}", s)
    }
}
