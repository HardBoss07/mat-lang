//! Target lvalue type resolution for assignments and mutations.
//!
//! This module resolves the static type of dotted field paths and tuple indices
//! occurring on the left-hand side of assignments and increments.

use crate::ast::{Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Resolves the static type of a dotted target path (e.g. `obj.field.subfield` or `tuple.0`).
    ///
    /// # Arguments
    ///
    /// * `target` - Dotted path string.
    /// * `symbols` - Active symbol table.
    /// * `span` - Source span of the assignment target.
    ///
    /// # Returns
    ///
    /// The resolved [`Type`] of the target lvalue.
    ///
    /// # Errors
    ///
    /// Returns a [`crate::error::MatcError`] if the variable or any field path is undefined or out of bounds.
    pub(crate) fn resolve_target_type(
        &self,
        target: &str,
        symbols: &SymbolTable,
        span: Span,
    ) -> Result<Type> {
        let parts: Vec<&str> = target.split('.').collect();
        let base_name = parts[0];
        let sym = symbols
            .lookup(base_name)
            .ok_or_else(|| self.undefined_variable(base_name, span))?;
        let mut current_ty = sym.ty.clone();

        for field in &parts[1..] {
            current_ty = self.resolve_field_target_type(&current_ty, field, symbols, span)?;
        }

        Ok(current_ty)
    }

    /// Resolves one level of field or tuple index access against a base type.
    fn resolve_field_target_type(
        &self,
        base_ty: &Type,
        field: &str,
        symbols: &SymbolTable,
        span: Span,
    ) -> Result<Type> {
        match base_ty {
            Type::Struct(struct_name) | Type::Custom(struct_name) => {
                let struct_def = symbols.lookup_struct(struct_name).ok_or_else(|| {
                    self.type_error(format!("Undefined struct '{}'", struct_name), span)
                })?;
                struct_def
                    .fields
                    .iter()
                    .find(|f| f.name == field)
                    .map(|f| f.ty.clone())
                    .ok_or_else(|| {
                        self.type_error(
                            format!("Struct '{}' has no field '{}'", struct_name, field),
                            span,
                        )
                    })
            }
            Type::Tuple(elem_types) => {
                let idx = field.parse::<usize>().map_err(|_| {
                    self.type_error(format!("Invalid tuple index '{}'", field), span)
                })?;
                elem_types.get(idx).cloned().ok_or_else(|| {
                    self.type_error(format!("Tuple index {} out of bounds", idx), span)
                })
            }
            other => Err(self.type_error(
                format!(
                    "Cannot access field '{}' on non-struct type {:?}",
                    field, other
                ),
                span,
            )),
        }
    }
}
