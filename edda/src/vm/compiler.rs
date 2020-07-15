//! Standard bytecode compiler

use thiserror::Error as ThisError;

use crate::ast::expressions::{Addition, Binary, BlockExpr, CallExpr, Expression, Grouping};
use crate::ast::statements::{FnDecl, Statement, VarDecl};
use crate::token::{Token, TokenType};
use crate::typer::{Type, TypeError};
use crate::vm::bytecode::{Chunk, OpCode};
use crate::vm::compiler::Local::FnReference;
use crate::vm::function::FunctionDeclaration;
use crate::Error;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(ThisError, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    TypeError(#[from] TypeError),
    #[error("too many locals")]
    TooManyLocals,
    #[error("type {0} doesn't have known size at compile time!")]
    UnknownSize(Type),
    #[error("could not resolve {0} to local variable")]
    UnrecognizedVariable(String),
    #[error("could not resolve {0} to a type")]
    UnrecognizedType(String),
    #[error("wrong number of arguments passed to {0}, expected {1}, {2} given")]
    MismatchedArity(String, usize, usize),
}

impl From<CompilerError> for Error {
    fn from(d: CompilerError) -> Self {
        Error::CompilerError(d)
    }
}

type CompilerResult = Result<Chunk, CompilerError>;

pub enum Local<'s> {
    Stacked(StackAllocated<'s>),
    FnReference(Rc<RefCell<FunctionDeclaration<'s>>>),
}

impl<'s> Local<'s> {
    pub fn name(&self) -> &str {
        match self {
            Local::Stacked(StackAllocated { name, .. }) => name.text,
            Local::FnReference(fn_ref) => {
                let fn_ref: &RefCell<FunctionDeclaration> = fn_ref.borrow();
                fn_ref.borrow().name.text
            }
        }
    }

    pub fn compile_time_size(&self) -> usize {
        match self {
            Local::Stacked(StackAllocated { kind, .. }) => kind.compile_time_size().unwrap(),
            Local::FnReference(..) => 2,
        }
    }

    pub fn kind(&self) -> Type {
        match self {
            Local::Stacked(StackAllocated { kind, .. }) => kind.clone(),
            Local::FnReference(fn_decl) => {
                let fn_ref: &RefCell<FunctionDeclaration> = fn_decl.borrow();
                let fn_ref = fn_ref.borrow();

                Type::Function(
                    fn_ref.signature.0.iter().map(|(_, k)| k.clone()).collect(),
                    Box::from(fn_ref.signature.1.clone()),
                )
            }
        }
    }
}

pub struct StackAllocated<'s> {
    pub name: Token<'s>,
    pub kind: Type,
    offset: usize,
}

impl<'s> From<StackAllocated<'s>> for Local<'s> {
    fn from(l: StackAllocated<'s>) -> Self {
        Local::Stacked(l)
    }
}

impl<'s> From<Rc<RefCell<FunctionDeclaration<'s>>>> for Local<'s> {
    fn from(l: Rc<RefCell<FunctionDeclaration<'s>>>) -> Self {
        Local::FnReference(l)
    }
}

pub struct Scope<'s> {
    locals: Vec<Local<'s>>,
    head: usize,
}

/// Lexical scope of current block.
#[must_use]
impl<'s> Scope<'s> {
    pub fn new(head: usize) -> Scope<'s> {
        Scope {
            locals: Vec::new(),
            head,
        }
    }

    /// Checks that we have space to allocate more locals
    fn assert_can_allocate(&self) -> Result<(), CompilerError> {
        if self.locals.len() >= 256 {
            Err(CompilerError::TooManyLocals)
        } else {
            Ok(())
        }
    }

    /// Returns pointer to start of this scope at the stack
    pub fn head(&self) -> usize {
        self.head
    }

    /// Adds a stack-allocated local binding.
    fn add_local(&mut self, identifier: &Token<'s>, kind: Type) -> Result<(), CompilerError> {
        let size = kind
            .compile_time_size()
            .ok_or_else(|| CompilerError::UnknownSize(kind.clone()))?;

        self.assert_can_allocate()?;

        self.locals.push(
            StackAllocated {
                name: identifier.clone(),
                kind,
                offset: self.head,
            }
            .into(),
        );

        self.head += size;

        Ok(())
    }

    fn add_function_ref(
        &mut self,
        identifier: &Token<'s>,
        function: Rc<RefCell<FunctionDeclaration<'s>>>,
    ) -> Result<(), CompilerError> {
        self.assert_can_allocate()?;
        let fn_local: Local = function.into();

        self.head += fn_local.compile_time_size();
        self.locals.push(fn_local);

        Ok(())
    }

    /// Consumes this scope, returning list of function declarations that need to be compiled.
    fn finish(self) -> Vec<Local<'s>> {
        self.locals
    }
}

pub struct Compiler<'s> {
    scopes: Vec<Scope<'s>>,
    chunk: Chunk,
    /// Collection of function declarations
    functions: Vec<Rc<RefCell<FunctionDeclaration<'s>>>>,
    /// Mapping of type names to types
    types: HashMap<String, Type>,
}

impl<'s> Compiler<'s> {
    pub fn new() -> Compiler<'s> {
        Compiler {
            scopes: vec![Scope::new(0)],
            chunk: Chunk::new(),
            functions: Vec::new(),
            types: [("i32", Type::I32), ("bool", Type::Boolean)]
                .iter()
                .map(|(id, t)| ((*id).to_owned(), t.clone()))
                .collect(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new(self.scope().head()));
    }

    fn push_scope_with_offset(&mut self, offset: usize) {
        self.scopes.push(Scope::new(offset));
    }

    fn pop_scope(&mut self, mut chunk: Chunk) -> Result<Chunk, CompilerError> {
        let locals = self.scopes.pop().unwrap().finish();

        for local in locals {
            let size = local.compile_time_size();

            if size > 0xFF_FF {
                unimplemented!();
            }

            // TODO: this can be optimized by merging POPN's
            chunk.push_op(OpCode::PopN);
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

    fn add_function(
        &mut self,
        function: FunctionDeclaration<'s>,
    ) -> Rc<RefCell<FunctionDeclaration<'s>>> {
        let decl = Rc::new(RefCell::new(function));
        self.functions.push(Rc::clone(&decl));

        decl
    }

    fn resolve_local(&self, identifier: Token<'s>) -> Result<&Local<'s>, CompilerError> {
        for scope in self.scopes.iter().rev() {
            // use reversed iterator of locals because we allow in-scope shadowing
            for local in scope.locals.iter().rev() {
                if local.name() == identifier.text {
                    return Ok(local);
                }
            }
        }

        Err(CompilerError::UnrecognizedVariable(
            identifier.text.to_owned(),
        ))
    }

    pub fn compile(mut self, script: Vec<Statement<'s>>, mut chunk: Chunk) -> CompilerResult {
        // compile statements
        for stmt in script {
            chunk = self.compile_statement(stmt, chunk)?;
        }

        // compile functions
        while self.functions.len() > 0 {
            let func = self.functions.remove(0);
            let offset = chunk.head();
            let func_borrow: &RefCell<FunctionDeclaration> = func.borrow();
            chunk = self.compile_function(&func_borrow, offset, chunk)?;

            for i in &func_borrow.borrow().references {
                if offset > 0xFF_FF {
                    unimplemented!("long func call");
                }

                chunk.code[*i..*i + 2].copy_from_slice(&(offset as u16).to_le_bytes());
            }
        }

        Ok(chunk)
    }

    fn compile_function(
        &mut self,
        function: &RefCell<FunctionDeclaration<'s>>,
        offset: usize,
        mut chunk: Chunk,
    ) -> Result<Chunk, CompilerError> {
        // When the function has been called, stack will contain it's parameter in the order
        // they were declared. We want to push a new scope, that overlaps with the last one.
        // This is fine, since the scope will get discarded, and no POPs will be generated,
        // as the RETURN opcode will take care of cleaning the stack.
        let body = {
            let mut func_mut = function.borrow_mut();
            func_mut.offset = Some(offset);
            func_mut.body.take().unwrap()
        };

        {
            let func_borrow = function.borrow();
            let mut stack_size = func_borrow
                .signature
                .0
                .iter()
                .map(|(_, k)| k.compile_time_size().unwrap())
                .sum();

            self.push_scope_with_offset(stack_size);

            for param in &func_borrow.signature.0 {
                self.scope_mut().add_local(&param.0, param.1.clone())?;
            }

            let actual_ret_type = self.infer_type(&body)?;

            if actual_ret_type != func_borrow.signature.1 {
                return Err(TypeError {
                    err: actual_ret_type,
                    expected: func_borrow.signature.1.clone(),
                }
                .into());
            }
        }

        chunk = self.compile_expr(*body, chunk)?;

        let locals = self.discard_scope()?.finish();

        chunk.push_op(OpCode::FnReturn);

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
                let (param_types, errs): (Vec<_>, Vec<_>) = params
                    .iter()
                    .map(|(id, k)| self.resolve_type(k).and_then(|k| Ok((id.clone(), k))))
                    .partition(Result::is_ok);

                if !errs.is_empty() {
                    // TODO: return multiple errors
                    return Err(errs.into_iter().map(Result::unwrap_err).next().unwrap());
                }

                let param_types = param_types.into_iter().map(Result::unwrap).collect();
                let ret_type = self.resolve_type(&ret_type)?;

                let fn_ref = self.add_function(FunctionDeclaration {
                    name: name.clone(),
                    signature: (param_types, ret_type),
                    body: Some(body),
                    references: vec![],
                    offset: None,
                });

                // we need to also create local binding with the function name
                self.scope_mut().add_function_ref(&name, fn_ref)?;

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
                    stack_size += local.compile_time_size();
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

                let local = self.resolve_local(identifier)?;
                let size = local.compile_time_size();

                match local {
                    Local::Stacked(StackAllocated { name, kind, offset }) => {
                        if size > 0xFF_FF || *offset > 0xFF_FF {
                            unimplemented!("long get local")
                        }

                        chunk.push_op(OpCode::GetLocal);
                        chunk.push_value((size as u16).to_le_bytes());
                        chunk.push_value((*offset as u16).to_le_bytes());
                    }
                    Local::FnReference(fn_ref) => {
                        // we could have a separate LoadFunction opcode, but it is just
                        // ConstantU16, so why bother (im lazy ok)
                        chunk.push_op(OpCode::LoadFunction);

                        let offset = { RefCell::borrow(fn_ref).offset.clone() };

                        if let Some(offset) = offset {
                            if offset > 0xFF_FF {
                                unimplemented!("long call");
                            }

                            chunk.push_value((offset as u16).to_le_bytes());
                        } else {
                            let backpatch_index = chunk.head();
                            chunk.push_value(0xAB_BAu16.to_le_bytes());

                            // save the position of the address so we can backpatch it once the function
                            // is actually compiled
                            fn_ref.borrow_mut().references.push(backpatch_index);
                        }
                    }
                }

                Ok(chunk)
            }
            Expression::Call(CallExpr { callee, arguments }) => {
                // When performing a call, stack should look like this
                // ...
                // | arg 1
                // | arg 2
                // | ... arg N
                // | callable
                //
                // so we must compile arguments before we compile the callee.
                // This has an interesting side effect when compiling something like this:
                // ```edda
                // fn foo(a: ()) -> () {}
                // fn bar(a: ()) -> () {}
                //
                // let mut ptr = foo;
                //
                // ptr({ ptr = bar: () });
                // ```
                //
                // Since argument expressions are executed before the callee expression,
                // `ptr` will actually have a reference to bar, instead of foo.
                // This might be confusing behaviour, but I'm not sure whats the best way to fix
                // this.

                let (arg_types, ret_type) = match self.infer_type(callee.as_ref()) {
                    Ok(Type::Function(arg_types, ret_type)) => Ok((arg_types, ret_type)),
                    Ok(other) => Err(TypeError {
                        err: other,
                        // TODO: the actual expected type could be inferred from context, but
                        //       im being lazy right now
                        expected: Type::Function(vec![Type::Any], Box::new(Type::Any)),
                    }
                    .into()),
                    Err(err) => Err(err),
                }?;

                if arg_types.len() != arguments.len() {
                    // TODO: actually figure the name of the function. This could get rather complex
                    return Err(CompilerError::MismatchedArity(
                        "?".to_owned(),
                        arg_types.len(),
                        arguments.len(),
                    ));
                }

                let mut args_size = 0;
                for (arg, kind) in arguments.into_iter().zip(arg_types.into_iter()) {
                    let actual_type = self.infer_type(&arg)?;

                    if actual_type != kind {
                        return Err(TypeError {
                            err: actual_type,
                            expected: kind,
                        }
                        .into());
                    }

                    chunk = self.compile_expr(arg, chunk)?;
                    args_size += kind.compile_time_size().unwrap();
                }

                if args_size > 0xFF_FF {
                    unimplemented!("long call stack");
                }

                let ret_size = ret_type
                    .compile_time_size()
                    .ok_or_else(|| CompilerError::UnknownSize(*ret_type))?;

                if ret_size > 0xFF_FF {
                    unimplemented!("long call stack");
                }

                chunk = self.compile_expr(*callee, chunk)?;
                chunk.push_op(OpCode::Call);
                chunk.push_value((args_size as u16).to_le_bytes());
                chunk.push_value((ret_size as u16).to_le_bytes());

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
            Expression::Variable(bind) => Ok(self.resolve_local(bind.0)?.kind().clone()),
            Expression::If(expr) => self.infer_type(expr.body.as_ref()),
            Expression::Block(block) => self.infer_type(block.ret.as_ref()),
            Expression::Call(CallExpr { callee, arguments }) => {
                let callee_kind = self.infer_type(callee)?;
                match callee_kind {
                    Type::Function(_, ret_type) => Ok(*ret_type.clone()),
                    other @ _ => Err(TypeError {
                        err: other,
                        expected: Type::Function(
                            arguments
                                .iter()
                                .map(|a| self.infer_type(a))
                                .collect::<Result<Vec<_>, CompilerError>>()?,
                            Box::from(Type::Any),
                        ),
                    }
                    .into()),
                }
            }
            e @ _ => unimplemented!("inference for {:?}", e),
        }
    }

    /// Resolves identifier token into a type, if possible
    pub fn resolve_type(&self, token: &Token<'s>) -> Result<Type, CompilerError> {
        let identifier = match token.t_type {
            TokenType::Identifier => token.text,
            _ => panic!("Attempted to resolve invalid token to type: {}", token),
        };

        self.types
            .get(identifier)
            .and_then(|t| Some(t.clone()))
            .ok_or(CompilerError::UnrecognizedType(identifier.to_owned()))
    }
}
