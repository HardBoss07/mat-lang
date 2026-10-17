//! Match statement pattern type checking and symbol binding.
//!
//! This module binds variables introduced by match arm patterns (Result `Ok`/`Err`,
//! enum tuple variants, and enum struct variants) into scope.

use crate::ast::{EnumVariantPayload, MatchArm, MatchPattern, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Type checks a match statement and validates each match arm.
    pub(crate) fn check_match_stmt(
        &self,
        expr: &crate::ast::Expression,
        arms: &[MatchArm],
        span: Span,
        current_return_type: &Type,
        symbols: &SymbolTable,
    ) -> Result<()> {
        let expr_ty = self.synthesize_expr(expr, symbols)?;
        for arm in arms {
            symbols.push_scope();
            self.bind_match_arm_pattern(&arm.pattern, &expr_ty, span, symbols)?;
            for stmt in &arm.body {
                self.check_statement(stmt, current_return_type, symbols)?;
            }
            symbols.pop_scope();
        }
        Ok(())
    }

    /// Binds variables from a match pattern into the active symbol scope.
    fn bind_match_arm_pattern(
        &self,
        pattern: &MatchPattern,
        expr_ty: &Type,
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<()> {
        match (pattern, expr_ty) {
            (MatchPattern::Ok(var_name), Type::Result(ok_ty, _)) => {
                symbols.insert(var_name.clone(), *ok_ty.clone(), false);
            }
            (MatchPattern::Err(var_name), Type::Result(_, err_ty)) => {
                symbols.insert(var_name.clone(), *err_ty.clone(), false);
            }
            (MatchPattern::EnumUnit { .. }, _) | (MatchPattern::Wildcard, _) => {}
            (
                MatchPattern::EnumTuple { variant, vars, .. },
                Type::Custom(enum_name) | Type::Struct(enum_name),
            ) => {
                self.bind_enum_tuple_pattern(enum_name, variant, vars, symbols);
            }
            (
                MatchPattern::EnumStruct {
                    variant, fields, ..
                },
                Type::Custom(enum_name) | Type::Struct(enum_name),
            ) => {
                self.bind_enum_struct_pattern(enum_name, variant, fields, symbols);
            }
            (MatchPattern::Literal(lit), target_ty) => {
                self.check_expr(lit, target_ty, symbols)?;
            }
            _ => {
                return Err(self.type_error(
                    format!(
                        "Pattern {:?} mismatch for matched type {:?}",
                        pattern, expr_ty
                    ),
                    span,
                ));
            }
        }
        Ok(())
    }

    /// Binds enum tuple-variant variables from the payload into scope.
    fn bind_enum_tuple_pattern(
        &self,
        enum_name: &str,
        variant: &str,
        vars: &[String],
        symbols: &SymbolTable,
    ) {
        let short_variant = variant.split("::").last().unwrap_or(variant);
        if let Some(enum_def) = symbols.lookup_enum(enum_name).cloned() {
            if let Some(variant_def) = enum_def
                .variants
                .iter()
                .find(|v| v.name == short_variant || v.name == variant)
            {
                if let EnumVariantPayload::Tuple(tys) = &variant_def.payload {
                    for (i, var) in vars.iter().enumerate() {
                        let ty = tys.get(i).cloned().unwrap_or(Type::Int);
                        symbols.insert(var.clone(), ty, false);
                    }
                    return;
                }
            }
        }
        for var in vars {
            symbols.insert(var.clone(), Type::Int, false);
        }
    }

    /// Binds enum struct-variant fields from the payload into scope.
    fn bind_enum_struct_pattern(
        &self,
        enum_name: &str,
        variant: &str,
        fields: &[(String, String)],
        symbols: &SymbolTable,
    ) {
        let short_variant = variant.split("::").last().unwrap_or(variant);
        if let Some(enum_def) = symbols.lookup_enum(enum_name).cloned() {
            if let Some(variant_def) = enum_def
                .variants
                .iter()
                .find(|v| v.name == short_variant || v.name == variant)
            {
                if let EnumVariantPayload::Struct(flds) = &variant_def.payload {
                    for (field_name, var) in fields {
                        let ty = flds
                            .iter()
                            .find(|f| f.name == *field_name)
                            .map(|f| f.ty.clone())
                            .unwrap_or(Type::Int);
                        symbols.insert(var.clone(), ty, false);
                    }
                    return;
                }
            }
        }
        for (_field_name, var) in fields {
            symbols.insert(var.clone(), Type::Int, false);
        }
    }
}
