//! Literal and collection expression type synthesis.
//!
//! This module provides synthesis routines for interpolated strings, tuple literals,
//! array literals, and vector literals.

use super::compatibility::types_compatible;
use crate::ast::{Expression, FormatSpecifier, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Synthesizes an interpolated string expression by verifying each interpolated subexpression.
    pub(crate) fn synthesize_interpolated_string(
        &self,
        parts: &[(Expression, FormatSpecifier)],
        symbols: &SymbolTable,
    ) -> Result<Type> {
        for (part, _specifier) in parts {
            self.synthesize_expr(part, symbols)?;
        }
        Ok(Type::String)
    }

    /// Synthesizes a tuple literal expression.
    pub(crate) fn synthesize_tuple_literal(
        &self,
        elements: &[Expression],
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let mut element_types = Vec::new();
        for element in elements {
            element_types.push(self.synthesize_expr(element, symbols)?);
        }
        Ok(Type::Tuple(element_types))
    }

    /// Synthesizes an array literal expression.
    pub(crate) fn synthesize_array_literal(
        &self,
        elements: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if elements.is_empty() {
            return Err(self.type_error(
                "Cannot infer type of empty array literal without explicit annotation",
                span,
            ));
        }
        let first_ty = self.synthesize_expr(&elements[0], symbols)?;
        for element in &elements[1..] {
            let elem_ty = self.synthesize_expr(element, symbols)?;
            if !types_compatible(&elem_ty, &first_ty) {
                return Err(self.type_error(
                    format!(
                        "Mismatched types in array literal: expected {:?}, got {:?}",
                        first_ty, elem_ty
                    ),
                    element.span(),
                ));
            }
        }
        Ok(Type::Array(Box::new(first_ty), elements.len()))
    }

    /// Synthesizes a vector literal expression.
    pub(crate) fn synthesize_vector_literal(
        &self,
        elements: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if elements.is_empty() {
            return Err(self.type_error(
                "Cannot infer type of empty vector literal without explicit annotation",
                span,
            ));
        }
        let first_ty = self.synthesize_expr(&elements[0], symbols)?;
        for element in &elements[1..] {
            let elem_ty = self.synthesize_expr(element, symbols)?;
            if !types_compatible(&elem_ty, &first_ty) {
                return Err(self.type_error(
                    format!(
                        "Mismatched types in vector literal: expected {:?}, got {:?}",
                        first_ty, elem_ty
                    ),
                    element.span(),
                ));
            }
        }
        Ok(Type::Vector(Box::new(first_ty)))
    }
}
