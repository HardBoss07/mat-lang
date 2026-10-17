//! Method call type checking for expressions.
//!
//! This module implements type verification for method invocations on receivers such as
//! vectors, arrays, and tuples.

use crate::ast::{Expression, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Checks the validity of a method call on a receiver expression.
    pub fn check_method_call(
        &self,
        receiver: &Expression,
        receiver_ty: &Type,
        method: &str,
        arguments: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        match (receiver_ty, method) {
            (Type::Vector(elem_ty), "push") => {
                self.check_vector_push(receiver, elem_ty, arguments, span, symbols)
            }
            (Type::Vector(elem_ty), "pop") => self.check_vector_pop(elem_ty, arguments, span),
            (Type::Vector(elem_ty), "get") => {
                self.check_vector_get(elem_ty, arguments, span, symbols)
            }
            (Type::Vector(_), "len") | (Type::Array(_, _), "len") | (Type::Tuple(_), "len") => {
                self.check_container_len(arguments, span)
            }
            _ => Err(self.type_error(
                format!("Method '{}' not found on type {:?}", method, receiver_ty),
                span,
            )),
        }
    }

    /// Verifies arguments for vector `push` method calls and performs lazy inference if needed.
    fn check_vector_push(
        &self,
        receiver: &Expression,
        elem_ty: &Type,
        arguments: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if arguments.is_empty() {
            return Err(self.type_error("push requires at least 1 argument", span));
        }

        let mut current_elem_ty = elem_ty.clone();

        for argument in arguments {
            if current_elem_ty == Type::Infer {
                let inferred_arg_ty = self.synthesize_expr(argument, symbols)?;
                current_elem_ty = inferred_arg_ty.clone();

                if let Expression::Identifier(var_name, _) = receiver {
                    symbols.update_type(var_name, Type::Vector(Box::new(inferred_arg_ty)));
                }
            } else {
                self.check_expr(argument, &current_elem_ty, symbols)?;
            }
        }
        Ok(Type::Void)
    }

    /// Verifies arguments for vector `pop` method calls.
    fn check_vector_pop(
        &self,
        elem_ty: &Type,
        arguments: &[Expression],
        span: Span,
    ) -> Result<Type> {
        if !arguments.is_empty() {
            return Err(self.type_error("pop takes 0 arguments", span));
        }
        Ok(elem_ty.clone())
    }

    /// Verifies arguments for vector `get` method calls.
    fn check_vector_get(
        &self,
        elem_ty: &Type,
        arguments: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if arguments.len() != 1 {
            return Err(self.type_error("get takes exactly 1 argument", span));
        }
        let index_ty = self.synthesize_expr(&arguments[0], symbols)?;
        if index_ty != Type::Int && index_ty != Type::I32 {
            return Err(self.type_error("get index must be integer", arguments[0].span()));
        }
        Ok(elem_ty.clone())
    }

    /// Verifies arguments for `len` method calls on containers.
    fn check_container_len(&self, arguments: &[Expression], span: Span) -> Result<Type> {
        if !arguments.is_empty() {
            return Err(self.type_error("len takes 0 arguments", span));
        }
        Ok(Type::Int)
    }
}
