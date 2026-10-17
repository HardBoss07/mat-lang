//! Collection and container type checking against expected target types.
//!
//! This module checks tuple literals, array literals, vector literals, and `Vec::new()` calls.

use crate::ast::{Expression, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Checks a tuple literal against expected tuple element types.
    pub(crate) fn check_tuple_literal(
        &self,
        elements: &[Expression],
        target_types: &[Type],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if elements.len() != target_types.len() {
            return Err(self.type_error(
                format!(
                    "Tuple length mismatch: expected {}, got {}",
                    target_types.len(),
                    elements.len()
                ),
                span,
            ));
        }
        let mut checked_types = Vec::new();
        for (element, target_element_ty) in elements.iter().zip(target_types.iter()) {
            checked_types.push(self.check_expr(element, target_element_ty, symbols)?);
        }
        Ok(Type::Tuple(checked_types))
    }

    /// Checks an array literal against expected element type and length.
    pub(crate) fn check_array_literal(
        &self,
        elements: &[Expression],
        target_elem_ty: &Type,
        expected_len: usize,
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if elements.len() != expected_len {
            return Err(self.type_error(
                format!(
                    "Array length mismatch: expected {}, got {}",
                    expected_len,
                    elements.len()
                ),
                span,
            ));
        }
        for element in elements {
            self.check_expr(element, target_elem_ty, symbols)?;
        }
        Ok(Type::Array(Box::new(target_elem_ty.clone()), expected_len))
    }

    /// Checks a vector literal against expected element type.
    pub(crate) fn check_vector_literal(
        &self,
        elements: &[Expression],
        target_elem_ty: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        for element in elements {
            self.check_expr(element, target_elem_ty, symbols)?;
        }
        Ok(Type::Vector(Box::new(target_elem_ty.clone())))
    }

    /// Checks a `Vec::new()` call expression against expected vector type.
    pub(crate) fn check_vec_new_call(
        &self,
        arguments: &[Expression],
        target_elem_ty: &Type,
        span: Span,
    ) -> Result<Type> {
        if !arguments.is_empty() {
            return Err(self.type_error("Vec::new() takes 0 arguments", span));
        }
        Ok(Type::Vector(Box::new(target_elem_ty.clone())))
    }
}
