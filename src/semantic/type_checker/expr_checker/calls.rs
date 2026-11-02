//! Call and invocation expression type synthesis.
//!
//! Coordinates user function invocations, runtime built-ins, tuple-variant enum calls,
//! and member accesses by delegating to specialized submodules.

pub mod access;
pub mod intrinsics;

#[allow(unused_imports)]
pub(crate) use access::*;
#[allow(unused_imports)]
pub(crate) use intrinsics::*;

use crate::ast::{EnumVariantPayload, Expression, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Synthesizes the type of a function or variant call expression.
    ///
    /// # Arguments
    ///
    /// * `callee` - Callee identifier or path.
    /// * `arguments` - Call argument expressions.
    /// * `span` - Source span of the call.
    /// * `symbols` - Active symbol table.
    ///
    /// # Errors
    ///
    /// Returns [`crate::error::MatcError`] if the callee is unknown or arguments violate parameter types.
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

    /// Synthesizes user-defined function calls, validating parameter count and individual argument types.
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

    /// Synthesizes enum tuple-variant constructor calls.
    fn synthesize_enum_tuple_call(
        &self,
        callee: &str,
        arguments: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        let (enum_name, variant_name) = match callee.rfind("::") {
            Some(idx) => (&callee[..idx], &callee[idx + 2..]),
            None => return Err(self.undefined_function(callee, span)),
        };

        let enum_def = match symbols.lookup_enum(enum_name).cloned() {
            Some(def) => def,
            None => return Err(self.undefined_function(callee, span)),
        };

        let variant = match enum_def.variants.iter().find(|v| v.name == variant_name) {
            Some(v) => v,
            None => return Err(self.undefined_function(callee, span)),
        };

        self.validate_enum_variant_arguments(
            enum_name,
            variant_name,
            &variant.payload,
            arguments,
            span,
            symbols,
        )?;
        Ok(Type::Custom(enum_name.to_string()))
    }

    /// Validates arguments against an enum variant's expected payload structure.
    fn validate_enum_variant_arguments(
        &self,
        enum_name: &str,
        variant_name: &str,
        payload: &EnumVariantPayload,
        arguments: &[Expression],
        span: Span,
        symbols: &SymbolTable,
    ) -> Result<()> {
        match payload {
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
        Ok(())
    }
}
