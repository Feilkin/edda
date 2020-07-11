//! Standard bytecode compiler

use thiserror::Error as ThisError;

use crate::ast::expressions::{Addition, Binary, BlockExpr, CallExpr, Expression, Grouping};
use crate::ast::statements::{BlockStmt, FnDecl, Statement, VarDecl};
use crate::token::{Token, TokenType};
use crate::typer::{AsEddaType, FromStack, Type, TypeError};
use crate::vm::bytecode::{Chunk, OpCode};
use crate::vm::function::{Function, Script};
use crate::Error;
use std::borrow::BorrowMut;
use std::collections::HashMap;

#[derive(ThisError, Debug)]
pub enum CompilerError {
    #[error("compile error: {0}")]
    TypeError(#[from] TypeError),
    #[error("could not resolve type {0}")]
    TypeNotFound(String),
    #[error("attempted to call type {0} which is not callable")]
    NotCallable(Type),
    #[error("too many locals")]
    TooManyLocals,
    #[error("type {0} doesn't have known size at compile time!")]
    UnknownSize(Type),
    #[error("could not resolve {0} to local variable")]
    UnrecognizedVariable(String),
    #[error("static function {0} already declared")]
    FuncAlreadyDeclared(String),
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
    type_defs: HashMap<String, Type>,
}

impl<'s> Compiler<'s> {
    pub fn new() -> Compiler<'s> {
        Compiler {
            scopes: vec![Scope::new(0)],
            type_defs: [("i32", Type::I32), ("bool", Type::Boolean)]
                .iter()
                .cloned()
                .map(|(n, t)| (n.to_owned(), t))
                .collect(),
        }
    }

    pub fn with_type(types: &HashMap<String, Type>) -> Compiler<'s> {
        Compiler {
            scopes: vec![Scope::new(0)],
            type_defs: types.clone(),
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

    /// discards (pops without freeing stack) current scope.
    fn discard_scope(&mut self) -> Result<Scope<'s>, CompilerError> {
        // we should never be out of stacks, so unwrap is ok here :)
        Ok(self.scopes.pop().unwrap())
    }

    fn scope(&self) -> &Scope<'s> {
        self.scopes.last().unwrap()
    }

    fn scope_mut(&mut self) -> &mut Scope<'s> {
        self.scopes.last_mut().unwrap()
    }

    fn resolve_local(&self, identifier: &Token<'s>) -> Result<(&Type, usize), CompilerError> {
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

    fn resolve_type(&self, identifier: &Token<'s>) -> Result<Type, CompilerError> {
        self.type_defs
            .get(identifier.text)
            .and_then(|t| Some(t.clone()))
            .ok_or_else(|| CompilerError::TypeNotFound(identifier.text.to_owned()))
    }

    pub fn compile<T: FromStack>(
        &mut self,
        script: BlockExpr<'s>,
    ) -> Result<Script<T>, CompilerError> {
        let script = script.into();
        let mut chunk = Chunk::new();
        // check that script will produce correct output
        let expected_kind = T::as_type();
        let actual_kind = self.infer_type(&script, &chunk)?;

        if actual_kind != expected_kind {
            Err(TypeError {
                err: actual_kind,
                expected: expected_kind,
            })?;
        }

        chunk = self.compile_expr(script, chunk)?;

        Ok(Script::new(chunk))
    }

    fn compile_function<'p>(
        &mut self,
        params: Vec<(Token<'s>, Type)>,
        body: Expression<'s>,
        parent: Chunk,
    ) -> CompilerResult {
        let mut chunk = Chunk::with_parent(Box::new(parent));

        for (id, kind) in params {
            self.scope_mut().add_local(&id, kind)?;
        }
        chunk = self.compile_expr(body, chunk)?;

        chunk.push_op(OpCode::Return);

        Ok(chunk)
    }

    fn compile_statement(&mut self, stmt: Statement<'s>, mut chunk: Chunk) -> CompilerResult {
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

                let kind = self.infer_type(&initializer, &chunk)?;
                chunk = self.compile_expr(*initializer, chunk)?;
                self.scope_mut().add_local(&identifier, kind)?;

                Ok(chunk)
            }
            Statement::Fn(FnDecl {
                name,
                params,
                body,
                ret_type,
            }) => {
                let param_types: Vec<Type> = params.iter().map(|_p| Type::I32).collect();
                let ret_type = self.resolve_type(&ret_type)?;
                let inferred_type = self.infer_type(&body, &chunk)?;

                if ret_type != inferred_type {
                    return Err(TypeError {
                        err: inferred_type,
                        expected: ret_type,
                    }
                    .into());
                }

                let index = chunk.declare_function(&name, (param_types.clone(), ret_type))?;

                let mut func_chunk = Compiler::with_type(&self.type_defs).compile_function(
                    params.into_iter().zip(param_types.into_iter()).collect(),
                    *body,
                    chunk,
                )?;

                chunk = *func_chunk.parent.take().unwrap();

                chunk.functions[index].borrow_mut().finish();

                Ok(chunk)
            }
            _ => unimplemented!(),
        }
    }

    fn compile_expr(&mut self, ast: Expression<'s>, mut chunk: Chunk) -> CompilerResult {
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
                let kind = self.infer_type(&ret, &chunk)?;

                // make a temp value which gets populated by the block value
                let size = kind
                    .compile_time_size()
                    .ok_or_else(|| CompilerError::UnknownSize(kind.clone()))?;

                self.push_scope();

                for stmt in statements {
                    chunk = self.compile_statement(stmt, chunk)?;
                }

                chunk = self.compile_expr(*ret, chunk)?;

                // instead of popping the scope, we use BlockReturn OpCode
                let scope = self.discard_scope()?;

                // we need to calculate size of the stack so we can free it
                let mut stack_size = 0;
                for local in scope.locals {
                    // stack locals always have known size
                    stack_size += local.kind.compile_time_size().unwrap();
                }

                if stack_size > 0xFF_FF {
                    unimplemented!("large block return");
                }

                // finally, we can make the BlockReturn opcode using size and stack_size
                chunk.push_op(OpCode::BlockReturn);
                chunk.push_value((size as u16).to_le_bytes());
                chunk.push_value((stack_size as u16).to_le_bytes());

                Ok(chunk)
            }
            Expression::If(if_expr) => {
                let cond_type = self.infer_type(if_expr.condition.as_ref(), &chunk)?;

                if cond_type != Type::Boolean {
                    return Err(TypeError {
                        err: cond_type,
                        expected: Type::Boolean,
                    }
                    .into());
                }

                let body_type = self.infer_type(if_expr.body.as_ref(), &chunk)?;
                let else_type = self.infer_type(if_expr.else_body.as_ref(), &chunk)?;

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

                match self.resolve_local(&identifier) {
                    Ok((kind, offset)) => {
                        let size = kind.compile_time_size().unwrap();

                        if size > 0xFF_FF || offset > 0xFF_FF {
                            unimplemented!("long get local")
                        }

                        chunk.push_op(OpCode::GetLocal);
                        chunk.push_value((size as u16).to_le_bytes());
                        chunk.push_value((offset as u16).to_le_bytes());

                        Ok(chunk)
                    }
                    Err(err) => {
                        // it might also be a function :shrug:
                        let (index, _) = chunk.resolve_function(&identifier).or(Err(err))?;

                        if index > 0xFF_FF {
                            // this would probably just be ConstantI32, but we haven't implemented
                            // the client side of this yes, so why bother :shrug:
                            unimplemented!("large function index");
                        }

                        chunk.push_op(OpCode::ConstantU16);
                        chunk.push_value((index as u16).to_le_bytes());

                        Ok(chunk)
                    }
                }
            }
            Expression::Call(CallExpr { callee, arguments }) => {
                let signature = match self.infer_type(&callee, &chunk)? {
                    Type::Function(params, ret) => Ok((params, ret)),
                    other @ _ => Err(CompilerError::NotCallable(other)),
                }?;

                for (arg, kind) in arguments.into_iter().zip(signature.0.iter()) {
                    let inferred = self.infer_type(&arg, &chunk)?;
                    if &inferred != kind {
                        return Err(TypeError {
                            err: inferred,
                            expected: kind.clone(),
                        }
                        .into());
                    }

                    chunk = self.compile_expr(arg, chunk)?;
                }

                chunk = self.compile_expr(*callee, chunk)?;
                chunk.push_op(OpCode::Call);

                Ok(chunk)
            }
            expr @ _ => unimplemented!("expression {:?} is not implemented yet", expr),
        }
    }
}

impl<'s> Compiler<'s> {
    pub fn infer_type(&self, expr: &Expression<'s>, chunk: &Chunk) -> Result<Type, CompilerError> {
        match expr {
            Expression::Equality(..) | Expression::Comparison(..) => Ok(Type::Boolean),
            Expression::Literal(..) => Ok(Type::I32),
            Expression::Addition(..) | Expression::Multiplication(..) => Ok(Type::I32),
            Expression::Group(inner) => self.infer_type(inner.inner.as_ref(), chunk),
            Expression::Variable(bind) => match self.resolve_local(&bind.0) {
                Ok((kind, _)) => Ok(kind.clone()),
                Err(err) => {
                    let (params, ret) = &chunk.resolve_function(&bind.0)?.1.signature;
                    Ok(Type::Function(params.clone(), Box::new(ret.clone())))
                }
            },
            Expression::If(expr) => self.infer_type(expr.body.as_ref(), chunk),
            Expression::Block(block) => self.infer_type(block.ret.as_ref(), chunk),
            e @ _ => unimplemented!("inference for {:?}", e),
        }
    }
}
