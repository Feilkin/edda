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
    Function(Vec<Type>, Box<Type>),
}

impl Type {
    pub fn compile_time_size(&self) -> Option<usize> {
        match self {
            Type::I32 => Some(4),
            Type::Boolean => Some(1),
            Type::Function(..) => None,
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
        };

        write!(f, "{}", s)
    }
}
