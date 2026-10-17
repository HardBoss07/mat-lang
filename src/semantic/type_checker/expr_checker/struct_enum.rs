//! Identifier and constructor expression type synthesis.
//!
//! This module provides specialized synthesis routines for identifiers, scoped enum variants,
//! struct initializers, and enum constructors.

use crate::ast::{EnumVariantPayload, Expression, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Synthesizes the type of an identifier or scoped enum variant path.
    pub(crate) fn synthesize_identifier_expr(
        &self,
        name: &str,
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if let Some(symbol) = symbols.lookup(name) {
            return Ok(symbol.ty.clone());
        }
        if name.contains("::") {
            return self.synthesize_scoped_identifier(name, span, symbols);
        }
        Err(self.undefined_variable(name, span))
    }

    /// Synthesizes a scoped identifier expression (e.g. `Color::Red`).
    fn synthesize_scoped_identifier(
        &self,
        name: &str,
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let parts: Vec<&str> = name.split("::").collect();
        let enum_name = parts[..parts.len() - 1].join("::");
        let variant_name = parts.last().unwrap();

        if let Some(enum_def) = symbols.lookup_enum(&enum_name) {
            if let Some(variant) = enum_def.variants.iter().find(|v| v.name == *variant_name) {
                if matches!(variant.payload, EnumVariantPayload::Unit) {
                    return Ok(Type::Custom(enum_name));
                }
            } else {
                return Err(self.type_error(
                    format!(
                        "Enum variant '{}' not found on enum '{}'",
                        variant_name, enum_name
                    ),
                    span,
                ));
            }
        }
        Err(self.undefined_variable(name, span))
    }

    /// Synthesizes the type of a struct or struct-variant initialization.
    pub(crate) fn synthesize_struct_init_expr(
        &self,
        name: &str,
        fields: &[(String, Expression)],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if name.contains("::") {
            if let Some(enum_ty) =
                self.try_synthesize_enum_struct_variant(name, fields, span, symbols)?
            {
                return Ok(enum_ty);
            }
        }

        let struct_def = symbols
            .lookup_struct(name)
            .ok_or_else(|| self.type_error(format!("Undefined struct '{}'", name), span))?
            .clone();

        if fields.len() != struct_def.fields.len() {
            return Err(self.type_error(
                format!(
                    "Struct '{}' expects {} fields, got {}",
                    name,
                    struct_def.fields.len(),
                    fields.len()
                ),
                span,
            ));
        }

        for (field_name, field_expr) in fields {
            let expected_field_ty = struct_def
                .fields
                .iter()
                .find(|field| field.name == *field_name)
                .map(|field| &field.ty)
                .ok_or_else(|| {
                    self.type_error(
                        format!("Unknown field '{}' on struct '{}'", field_name, name),
                        field_expr.span(),
                    )
                })?;
            self.check_expr(field_expr, expected_field_ty, symbols)?;
        }

        Ok(Type::Struct(name.to_string()))
    }

    /// Attempts to synthesize a struct-variant payload on an enum.
    fn try_synthesize_enum_struct_variant(
        &self,
        name: &str,
        fields: &[(String, Expression)],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Option<Type>> {
        let parts: Vec<&str> = name.split("::").collect();
        let enum_name = parts[..parts.len() - 1].join("::");
        let variant_name = parts.last().unwrap();

        if let Some(enum_def) = symbols.lookup_enum(&enum_name).cloned() {
            if let Some(variant) = enum_def.variants.iter().find(|v| v.name == *variant_name) {
                if let EnumVariantPayload::Struct(expected_fields) = &variant.payload {
                    if fields.len() != expected_fields.len() {
                        return Err(self.type_error(
                            format!(
                                "Enum variant '{}::{}' expects {} fields, got {}",
                                enum_name,
                                variant_name,
                                expected_fields.len(),
                                fields.len()
                            ),
                            span,
                        ));
                    }

                    for (field_name, field_expr) in fields {
                        let expected_field_ty = expected_fields
                            .iter()
                            .find(|field| field.name == *field_name)
                            .map(|field| &field.ty)
                            .ok_or_else(|| {
                                self.type_error(
                                    format!(
                                        "Unknown field '{}' on enum variant '{}::{}'",
                                        field_name, enum_name, variant_name
                                    ),
                                    field_expr.span(),
                                )
                            })?;
                        self.check_expr(field_expr, expected_field_ty, symbols)?;
                    }

                    return Ok(Some(Type::Custom(enum_name)));
                }
            }
        }
        Ok(None)
    }

    /// Synthesizes the type of an enum initialization expression.
    pub(crate) fn synthesize_enum_init_expr(
        &self,
        enum_name: Option<&str>,
        variant_name: &str,
        payload: &Option<Box<Expression>>,
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let effective_name = match enum_name {
            Some(name) => name.to_string(),
            None => variant_name.to_string(),
        };

        let enum_def = symbols
            .lookup_enum(&effective_name)
            .ok_or_else(|| self.type_error(format!("Undefined enum '{}'", effective_name), span))?
            .clone();

        let _variant = enum_def
            .variants
            .iter()
            .find(|variant| variant.name == variant_name)
            .ok_or_else(|| {
                self.type_error(
                    format!(
                        "Enum '{}' has no variant '{}'",
                        effective_name, variant_name
                    ),
                    span,
                )
            })?;

        if let Some(payload_expr) = payload {
            self.synthesize_expr(payload_expr, symbols)?;
        }

        Ok(Type::Custom(effective_name))
    }
}
