//! Call and access expression type synthesis.
//!
//! This module provides synthesis routines for function invocations, runtime built-ins,
//! tuple-variant enum calls, array indexing, tuple indexing, and field accesses.

use crate::ast::{EnumVariantPayload, Expression, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Synthesizes the type of a function or variant call expression.
    pub(crate) fn synthesize_call_expr(
        &self,
        callee: &str,
        arguments: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if callee == "Vec::new" {
            return Ok(Type::Vector(Box::new(Type::Infer)));
        }

        if callee == "println" || callee == "print" {
            return self.synthesize_print_call(arguments, symbols);
        }

        if let Some(fn_symbol) = symbols.lookup_function(callee).cloned() {
            return self.synthesize_user_function_call(
                callee,
                &fn_symbol.param_types,
                &fn_symbol.return_type,
                arguments,
                span,
                symbols,
            );
        }

        if callee.starts_with("_mat_rt_") {
            return self.synthesize_runtime_call(callee, arguments, symbols);
        }

        if callee.contains("::") {
            return self.synthesize_enum_tuple_call(callee, arguments, span, symbols);
        }

        Err(self.undefined_function(callee, span))
    }

    /// Synthesizes print/println built-in calls.
    fn synthesize_print_call(
        &self,
        arguments: &[Expression],
        symbols: &SymbolTable,
    ) -> Result<Type> {
        for argument in arguments {
            self.synthesize_expr(argument, symbols)?;
        }
        Ok(Type::Void)
    }

    /// Synthesizes user-defined function calls.
    fn synthesize_user_function_call(
        &self,
        callee: &str,
        param_types: &[Type],
        return_type: &Type,
        arguments: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        if arguments.len() != param_types.len() {
            return Err(self.type_error(
                format!(
                    "Function '{}' expects {} arguments, got {}",
                    callee,
                    param_types.len(),
                    arguments.len()
                ),
                span,
            ));
        }

        for (argument, param_ty) in arguments.iter().zip(param_types.iter()) {
            self.check_expr(argument, param_ty, symbols)?;
        }

        Ok(return_type.clone())
    }

    /// Synthesizes MAT runtime library calls.
    fn synthesize_runtime_call(
        &self,
        callee: &str,
        arguments: &[Expression],
        symbols: &SymbolTable,
    ) -> Result<Type> {
        for argument in arguments {
            self.synthesize_expr(argument, symbols)?;
        }

        if callee.contains("read_line") || callee.contains("read_to_string") {
            Ok(Type::String)
        } else if callee.contains("open")
            || callee.contains("write")
            || callee.contains("seek")
            || callee.contains("abs_int")
        {
            Ok(Type::Int)
        } else if callee.contains("flush") || callee.contains("close") {
            Ok(Type::Bool)
        } else if callee.contains("print") {
            Ok(Type::Void)
        } else {
            Ok(Type::F64)
        }
    }

    /// Synthesizes enum tuple-variant constructor calls.
    fn synthesize_enum_tuple_call(
        &self,
        callee: &str,
        arguments: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let parts: Vec<&str> = callee.split("::").collect();
        let enum_name = parts[..parts.len() - 1].join("::");
        let variant_name = parts.last().unwrap();

        if let Some(enum_def) = symbols.lookup_enum(&enum_name).cloned() {
            if let Some(variant) = enum_def.variants.iter().find(|v| v.name == *variant_name) {
                match &variant.payload {
                    EnumVariantPayload::Tuple(expected_types) => {
                        if arguments.len() != expected_types.len() {
                            return Err(self.type_error(
                                format!(
                                    "Enum variant '{}::{}' expects {} arguments, got {}",
                                    enum_name,
                                    variant_name,
                                    expected_types.len(),
                                    arguments.len()
                                ),
                                span,
                            ));
                        }
                        for (argument, expected_ty) in arguments.iter().zip(expected_types.iter()) {
                            self.check_expr(argument, expected_ty, symbols)?;
                        }
                    }
                    EnumVariantPayload::Unit => {
                        if !arguments.is_empty() {
                            return Err(self.type_error(
                                format!(
                                    "Unit enum variant '{}::{}' takes 0 arguments",
                                    enum_name, variant_name
                                ),
                                span,
                            ));
                        }
                    }
                    EnumVariantPayload::Struct(_) => {}
                }
                return Ok(Type::Custom(enum_name));
            }
        }

        Err(self.undefined_function(callee, span))
    }

    /// Synthesizes field access expressions on struct instances.
    pub(crate) fn synthesize_field_access(
        &self,
        expr: &Expression,
        field: &str,
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let expr_ty = self.synthesize_expr(expr, symbols)?;
        let struct_name = match &expr_ty {
            Type::Struct(name) | Type::Custom(name) => name,
            other => {
                return Err(self.type_error(
                    format!(
                        "Cannot access field '{}' on non-struct type {:?}",
                        field, other
                    ),
                    expr.span(),
                ));
            }
        };

        let struct_def = symbols
            .lookup_struct(struct_name)
            .ok_or_else(|| self.type_error(format!("Undefined struct '{}'", struct_name), span))?;

        let field_symbol = struct_def
            .fields
            .iter()
            .find(|f| f.name == field)
            .ok_or_else(|| {
                self.type_error(
                    format!("Struct '{}' has no field '{}'", struct_name, field),
                    span,
                )
            })?;

        if struct_name.contains("::") && !field_symbol.is_pub {
            return Err(self.type_error(
                format!(
                    "Undefined private field '{}' on struct '{}'",
                    field, struct_name
                ),
                span,
            ));
        }

        Ok(field_symbol.ty.clone())
    }

    /// Synthesizes array and vector indexing expressions.
    pub(crate) fn synthesize_array_access(
        &self,
        expr: &Expression,
        index: &Expression,
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let expr_ty = self.synthesize_expr(expr, symbols)?;
        let index_ty = self.synthesize_expr(index, symbols)?;
        if index_ty != Type::Int && index_ty != Type::I32 {
            return Err(self.type_error(
                format!("Array index must be an integer, got {:?}", index_ty),
                index.span(),
            ));
        }
        match expr_ty {
            Type::Array(elem_ty, _) => Ok(*elem_ty),
            Type::Vector(elem_ty) => Ok(*elem_ty),
            other => Err(self.type_error(format!("Cannot index non-array type {:?}", other), span)),
        }
    }

    /// Synthesizes tuple indexing expressions.
    pub(crate) fn synthesize_tuple_access(
        &self,
        expr: &Expression,
        index: usize,
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let expr_ty = self.synthesize_expr(expr, symbols)?;
        match expr_ty {
            Type::Tuple(types) => types.get(index).cloned().ok_or_else(|| {
                self.type_error(
                    format!(
                        "Tuple index {} out of bounds for tuple length {}",
                        index,
                        types.len()
                    ),
                    span,
                )
            }),
            other => Err(self.type_error(
                format!("Cannot access tuple index on non-tuple type {:?}", other),
                expr.span(),
            )),
        }
    }
}
