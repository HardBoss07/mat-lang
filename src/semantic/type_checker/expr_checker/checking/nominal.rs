//! Nominal type checking for structs, enums, and nominal call expressions.

use super::super::compatibility::types_compatible;
use crate::ast::{Expression, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Checks a call expression returning a custom or struct type against expected target.
    pub(crate) fn check_nominal_call(
        &self,
        expr: &Expression,
        callee: &str,
        target_ty: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let target_name = match target_ty {
            Type::Custom(name) | Type::Struct(name) => name,
            _ => unreachable!(),
        };
        let target_short = target_name.split("::").last().unwrap_or(target_name);
        if callee.contains("::") {
            let parts: Vec<&str> = callee.split("::").collect();
            let enum_name = parts[..parts.len() - 1].join("::");
            let enum_short = enum_name.split("::").last().unwrap_or(&enum_name);

            if enum_short == target_short {
                self.synthesize_expr(expr, symbols)?;
                return Ok(target_ty.clone());
            }
        }
        let synthesized = self.synthesize_expr(expr, symbols)?;
        if types_compatible(&synthesized, target_ty) {
            Ok(target_ty.clone())
        } else {
            Err(self.type_error(
                format!(
                    "Type mismatch: expected {:?}, got {:?}",
                    target_ty, synthesized
                ),
                expr.span(),
            ))
        }
    }

    /// Checks a struct init expression against expected nominal type.
    pub(crate) fn check_struct_init_target(
        &self,
        expr: &Expression,
        name: &str,
        target_ty: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let target_name = match target_ty {
            Type::Custom(target_name) | Type::Struct(target_name) => target_name,
            _ => unreachable!(),
        };
        let synthesized = self.synthesize_expr(expr, symbols)?;
        if types_compatible(&synthesized, target_ty) {
            Ok(target_ty.clone())
        } else {
            Err(self.type_error(
                format!(
                    "Type mismatch in struct initialization: expected {}, got {}",
                    target_name, name
                ),
                expr.span(),
            ))
        }
    }

    /// Checks an enum initialization expression against expected nominal type.
    pub(crate) fn check_enum_init_target(
        &self,
        enum_name: Option<&str>,
        payload: &Option<Box<Expression>>,
        span: Span,
        target_ty: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let target_name = match target_ty {
            Type::Custom(target_name) | Type::Struct(target_name) => target_name,
            _ => unreachable!(),
        };
        let target_short = target_name.split("::").last().unwrap_or(target_name);
        let effective_enum_name = enum_name.unwrap_or(target_short);
        let enum_short = effective_enum_name
            .split("::")
            .last()
            .unwrap_or(effective_enum_name);
        if enum_short != target_short {
            return Err(self.type_error(
                format!(
                    "Type mismatch in enum initialization: expected {}, got {}",
                    target_name, effective_enum_name
                ),
                span,
            ));
        }
        if let Some(payload_expr) = payload {
            self.synthesize_expr(payload_expr, symbols)?;
        }
        Ok(target_ty.clone())
    }

    /// Fallback check delegating to expression synthesis and compatibility validation.
    pub(crate) fn check_fallback_expr(
        &self,
        expr: &Expression,
        target_ty: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let synthesized = self.synthesize_expr(expr, symbols)?;
        if types_compatible(&synthesized, target_ty) {
            Ok(if *target_ty == Type::Infer {
                synthesized
            } else {
                target_ty.clone()
            })
        } else {
            Err(self.type_error(
                format!(
                    "Type mismatch: expected {:?}, got {:?}",
                    target_ty, synthesized
                ),
                expr.span(),
            ))
        }
    }
}
