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

#[derive(Debug, PartialEq)]
pub enum Type {
    I32,
    Boolean,
    Function(Vec<Type>, Box<Type>),
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

pub struct Typer {}

impl Typer {
    pub fn new() -> Typer {
        Typer {}
    }

    pub fn infer_type(&self, expr: &Expression) -> Result<Type, TypeError> {
        match expr {
            Expression::Equality(..) | Expression::Comparison(..) => Ok(Type::Boolean),
            Expression::Literal(..) => Ok(Type::I32),
            Expression::Addition(..) | Expression::Multiplication(..) => Ok(Type::I32),
            Expression::Group(inner) => self.infer_type(inner.inner.as_ref()),
            _ => unimplemented!(),
        }
    }
}