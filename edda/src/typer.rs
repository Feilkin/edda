//! Edda type system

use thiserror::Error;

use crate::{Expression, Vm};
use std::convert::TryInto;
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
    /// Empty type. Note that is not the same as "null" in some other, inferior languages, since
    /// edda does not have the concept of null. Nil means empty in type level, it has no possible
    /// values. Think of it as void return type.
    Nil,
}

impl Type {
    pub fn compile_time_size(&self) -> Option<usize> {
        match self {
            Type::I32 => Some(4),
            Type::Boolean => Some(1),
            Type::Function(..) => None,
            Type::Nil => Some(0),
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
            Type::Nil => "()".to_owned(),
        };

        write!(f, "{}", s)
    }
}

pub trait FromStack: Sized + AsEddaType {
    fn pop(vm: &mut Vm<Self>) -> Self;
}

pub trait AsEddaType {
    fn as_type() -> Type;
}

impl AsEddaType for i32 {
    fn as_type() -> Type {
        Type::I32
    }
}

impl FromStack for i32 {
    fn pop(vm: &mut Vm<i32>) -> Self {
        i32::from_le_bytes(vm.pop_bytes(2).unwrap().try_into().unwrap())
    }
}
