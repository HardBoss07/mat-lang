//! Literal and constructor type checking against expected target types.
//!
//! This module checks integer literals, float literals, Result constructors (`Ok`/`Err`),
//! and binary expressions against target types.

use super::super::compatibility::types_compatible;
use crate::ast::{BinaryOp, Expression, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Checks an `Ok(...)` result constructor expression.
    pub(crate) fn check_ok_constructor(
        &self,
        value: &Expression,
        ok_ty: &Type,
        target: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        self.check_expr(value, ok_ty, symbols)?;
        Ok(target.clone())
    }

    /// Checks an `Err(...)` result constructor expression.
    pub(crate) fn check_err_constructor(
        &self,
        value: &Expression,
        err_ty: &Type,
        target: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        self.check_expr(value, err_ty, symbols)?;
        Ok(target.clone())
    }

    /// Checks integer literal values against bounded integer types.
    pub(crate) fn check_int_literal(
        &self,
        value: i64,
        span: Span,
        target_ty: &Type,
    ) -> Result<Type> {
        match target_ty {
            Type::Int | Type::Infer => Ok(Type::Int),
            Type::I32 => {
                if value >= i32::MIN as i64 && value <= i32::MAX as i64 {
                    Ok(Type::I32)
                } else {
                    Err(self.type_error(
                        format!("Integer literal {} exceeds bounds for i32", value),
                        span,
                    ))
                }
            }
            Type::I16 => {
                if value >= i16::MIN as i64 && value <= i16::MAX as i64 {
                    Ok(Type::I16)
                } else {
                    Err(self.type_error(
                        format!("Integer literal {} exceeds bounds for i16", value),
                        span,
                    ))
                }
            }
            Type::I8 => {
                if value >= i8::MIN as i64 && value <= i8::MAX as i64 {
                    Ok(Type::I8)
                } else {
                    Err(self.type_error(
                        format!("Integer literal {} exceeds bounds for i8", value),
                        span,
                    ))
                }
            }
            other => Err(self.type_error(
                format!("Cannot check integer literal against type {:?}", other),
                span,
            )),
        }
    }

    /// Checks floating point literal values against precision types.
    pub(crate) fn check_float_literal(
        &self,
        _value: f64,
        span: Span,
        target_ty: &Type,
    ) -> Result<Type> {
        match target_ty {
            Type::F64 | Type::Infer => Ok(Type::F64),
            Type::F32 => Ok(Type::F32),
            other => Err(self.type_error(
                format!("Cannot check float literal against type {:?}", other),
                span,
            )),
        }
    }

    /// Checks a binary operation expression against an expected target type.
    pub(crate) fn check_binary_expr(
        &self,
        _expr: &Expression,
        op: &BinaryOp,
        left: &Expression,
        right: &Expression,
        target_ty: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let left_ty = self.check_expr(left, target_ty, symbols)?;
        let right_ty = self.check_expr(right, target_ty, symbols)?;
        if !types_compatible(&left_ty, &right_ty) {
            return Err(self.type_error(
                format!(
                    "Binary operator type mismatch: expected {:?}, got {:?}",
                    left_ty, right_ty
                ),
                left.span(),
            ));
        }
        match op {
            BinaryOp::Eq
            | BinaryOp::Neq
            | BinaryOp::Lt
            | BinaryOp::Lte
            | BinaryOp::Gt
            | BinaryOp::Gte
            | BinaryOp::And
            | BinaryOp::Or => Ok(Type::Bool),
            _ => Ok(target_ty.clone()),
        }
    }
}
