//! Standard bytecode compiler

use thiserror::Error as ThisError;

use crate::ast::expressions::{Addition, Binary, BlockExpr, Expression, Grouping};
use crate::ast::statements::{Statement, VarDecl};
use crate::token::{Token, TokenType};
use crate::typer::{Type, TypeError};
use crate::vm::bytecode::{Chunk, OpCode};
use crate::Error;

#[derive(ThisError, Debug)]
pub enum CompilerError {
    #[error("compile error: {0}")]
    TypeError(#[from] TypeError),
    #[error("too many locals")]
    TooManyLocals,
    #[error("type {0} doesn't have known size at compile time!")]
    UnknownSize(Type),
    #[error("could not resolve {0} to local variable")]
    UnrecognizedVariable(String),
}

impl From<CompilerError> for Error {
    fn from(d: CompilerError) -> Self {
        Error::CompilerError(d)
    }
}

type CompilerResult = Result<Chunk, CompilerError>;

pub struct Local<'s> {
    pub name: Token<'s>,
    pub kind: Type,
    offset: usize,
}

pub struct Scope<'s> {
    pub locals: Vec<Local<'s>>,
    pub head: usize,
}

impl<'s> Scope<'s> {
    pub fn new(head: usize) -> Scope<'s> {
        Scope {
            locals: Vec::new(),
            head,
        }
    }

    fn add_local(&mut self, identifier: &Token<'s>, kind: Type) -> Result<(), CompilerError> {
        let size = kind
            .compile_time_size()
            .ok_or_else(|| CompilerError::UnknownSize(kind.clone()))?;

        if self.locals.len() >= 256 {
            return Err(CompilerError::TooManyLocals);
        }

        self.locals.push(Local {
            name: identifier.clone(),
            kind,
            offset: self.head,
        });

        self.head += size;

        Ok(())
    }
}

pub struct Compiler<'s> {
    scopes: Vec<Scope<'s>>,
}

impl<'s> Compiler<'s> {
    pub fn new() -> Compiler<'s> {
        Compiler {
            scopes: vec![Scope::new(0)],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new(self.scope().head));
    }

    fn pop_scope(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let popped = self.scopes.pop().unwrap();

        for local in popped.locals {
            chunk.push_op(OpCode::PopN);
            let size = local
                .kind
                .compile_time_size()
                .expect("stack values should have known size!");

            if size > 0xFF_FF {
                unimplemented!();
            }

            // TODO: this can be optimized by merging POPN's
            chunk.push_value((size as u16).to_le_bytes());
        }

        Ok(chunk)
    }

    fn scope(&self) -> &Scope<'s> {
        self.scopes.last().unwrap()
    }

    fn scope_mut(&mut self) -> &mut Scope<'s> {
        self.scopes.last_mut().unwrap()
    }

    fn resolve_local(&self, identifier: Token<'s>) -> Result<(&Type, usize), CompilerError> {
        for scope in self.scopes.iter().rev() {
            for local in &scope.locals {
                if local.name.text == identifier.text {
                    return Ok((&local.kind, local.offset));
                }
            }
        }

        Err(CompilerError::UnrecognizedVariable(
            identifier.text.to_owned(),
        ))
    }

    pub fn compile(&mut self, script: Vec<Statement<'s>>, mut chunk: Chunk) -> CompilerResult {
        for stmt in script {
            chunk = self.compile_statement(stmt, chunk)?;
        }

        Ok(chunk)
    }

    pub fn compile_statement(&mut self, stmt: Statement<'s>, mut chunk: Chunk) -> CompilerResult {
        match stmt {
            Statement::Return(ret) => {
                if let Some(expr) = ret.value {
                    chunk = self.compile_expr(*expr, chunk)?;
                }

                chunk.push_op(OpCode::Return);
                Ok(chunk)
            }
            Statement::VarDecl(decl) => {
                let VarDecl {
                    identifier,
                    initializer,
                } = decl;

                let kind = self.infer_type(&initializer)?;
                self.scope_mut().add_local(&identifier, kind)?;
                chunk = self.compile_expr(*initializer, chunk)?;

                Ok(chunk)
            }
            _ => unimplemented!(),
        }
    }

    pub fn compile_expr(&mut self, ast: Expression<'s>, mut chunk: Chunk) -> CompilerResult {
        match ast {
            Expression::Literal(lit) => {
                let val: i32 = lit.token().text.parse().expect("failed to parse int :(");

                chunk.push_op(OpCode::ConstantI32);
                chunk.push_value(val.to_le_bytes());

                Ok(chunk)
            }
            Expression::Addition(expr) => {
                let Binary { lhs, operator, rhs } = expr.0;

                chunk = self.compile_expr(*lhs, chunk)?;
                chunk = self.compile_expr(*rhs, chunk)?;

                let op = match operator.t_type {
                    TokenType::Plus => OpCode::AddI32,
                    TokenType::Minus => OpCode::SubI32,
                    _ => unreachable!(),
                };

                chunk.push_op(op);

                Ok(chunk)
            }
            Expression::Multiplication(expr) => {
                let Binary { lhs, operator, rhs } = expr.0;

                chunk = self.compile_expr(*lhs, chunk)?;
                chunk = self.compile_expr(*rhs, chunk)?;

                let op = match operator.t_type {
                    TokenType::Star => OpCode::MulI32,
                    TokenType::Slash => OpCode::DivI32,
                    _ => unreachable!(),
                };

                chunk.push_op(op);

                Ok(chunk)
            }
            Expression::Group(expr) => {
                let Grouping { inner } = expr;

                self.compile_expr(*inner, chunk)
            }
            Expression::Block(block) => {
                let BlockExpr { statements, ret } = block;
                let kind = self.infer_type(&ret)?;

                // make a temp value which gets populated by the block value
                let size = kind
                    .compile_time_size()
                    .ok_or(|| CompilerError::UnknownSize(kind.clone()));

                if size > 0xFF_FF {
                    unimplemented!("long push n");
                }

                chunk.push_op(OpCode::PushN);
                chunk.push_value((size as u16).to_le_bytes());

                self.push_scope();

                for stmt in statements {
                    chunk = self.compile_statement(stmt, chunk)?;
                }

                chunk = self.pop_scope(chunk)?;

                Ok(chunk)
            }
            Expression::If(if_expr) => {
                let cond_type = self.infer_type(if_expr.condition.as_ref())?;

                if cond_type != Type::Boolean {
                    return Err(TypeError {
                        err: cond_type,
                        expected: Type::Boolean,
                    }
                    .into());
                }

                let body_type = self.infer_type(if_expr.body.as_ref())?;
                let else_type = self.infer_type(if_expr.else_body.as_ref())?;

                if body_type != else_type {
                    Err(TypeError {
                        err: else_type,
                        expected: body_type,
                    }
                    .into())
                } else {
                    chunk = self.compile_expr(*if_expr.condition, chunk)?;
                    chunk.push_op(OpCode::JumpIfFalse);

                    // we need to backpatch jump location
                    let then_jump = chunk.head();
                    chunk.push_value(0xFF_FF_u16.to_le_bytes());
                    chunk = self.compile_expr(*if_expr.body, chunk)?;

                    // if expressions always have else, so push jump and
                    chunk.push_op(OpCode::Jump);
                    let else_jump = chunk.code.len();
                    chunk.push_value(0xFF_FF_u16.to_le_bytes());

                    // backpatch then jump
                    let then_relative = chunk.head_short() - then_jump as u16;
                    chunk.set_value(then_relative.to_le_bytes(), then_jump);

                    chunk = self.compile_expr(*if_expr.else_body, chunk)?;

                    // backpatch else jump
                    let else_relative = chunk.head_short() - else_jump as u16;
                    chunk.set_value(else_relative.to_le_bytes(), else_jump);

                    Ok(chunk)
                }
            }
            Expression::Equality(expr) => {
                let Binary { lhs, operator, rhs } = expr.0;

                chunk = self.compile_expr(*lhs, chunk)?;
                chunk = self.compile_expr(*rhs, chunk)?;

                chunk.push_op(OpCode::EqualI32);

                Ok(chunk)
            }
            Expression::Comparison(expr) => {
                let Binary { lhs, operator, rhs } = expr.0;

                // swap lhs and rhs to save opcodes :)
                let (lhs, rhs) = match operator.t_type {
                    TokenType::Greater | TokenType::GreaterEqual => (rhs, lhs),
                    _ => (lhs, rhs),
                };

                chunk = self.compile_expr(*lhs, chunk)?;
                chunk = self.compile_expr(*rhs, chunk)?;

                let op = match operator.t_type {
                    TokenType::Less | TokenType::GreaterEqual => OpCode::LessI32,
                    TokenType::LessEqual | TokenType::Greater => OpCode::LessEqualI32,
                    _ => unreachable!(),
                };

                chunk.push_op(op);

                Ok(chunk)
            }
            Expression::Variable(bind) => {
                let identifier = bind.0;

                let (kind, offset) = self.resolve_local(identifier)?;
                let size = kind.compile_time_size().unwrap();

                if size > 0xFF_FF || offset > 0xFF_FF {
                    unimplemented!("long get local")
                }

                chunk.push_op(OpCode::GetLocal);
                chunk.push_value((size as u16).to_le_bytes());
                chunk.push_value((offset as u16).to_le_bytes());

                Ok(chunk)
            }
            expr @ _ => unimplemented!("expression {:?} is not implemented yet", expr),
        }
    }
}

impl<'s> Compiler<'s> {
    pub fn infer_type(&self, expr: &Expression<'s>) -> Result<Type, CompilerError> {
        match expr {
            Expression::Equality(..) | Expression::Comparison(..) => Ok(Type::Boolean),
            Expression::Literal(..) => Ok(Type::I32),
            Expression::Addition(..) | Expression::Multiplication(..) => Ok(Type::I32),
            Expression::Group(inner) => self.infer_type(inner.inner.as_ref()),
            Expression::Variable(bind) => Ok(self.resolve_local(bind.0)?.0.clone()),
            Expression::If(expr) => self.infer_type(expr.body.as_ref()),
            Expression::Block(block) => self.infer_type(block.ret.as_ref()),
            e @ _ => unimplemented!("inference for {:?}", e),
        }
    }
}
