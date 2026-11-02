//! Field, array, and tuple member access type synthesis.
//!
//! Checks and derives types for member accesses (`expr.field`, `arr[idx]`, `tuple.0`).

use crate::ast::{Expression, Span, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Synthesizes field access expressions on struct instances.
    ///
    /// # Arguments
    ///
    /// * `expr` - Struct instance expression.
    /// * `field` - Field name to inspect.
    /// * `span` - Source span of the access.
    /// * `symbols` - Active symbol table.
    ///
    /// # Errors
    ///
    /// Returns [`crate::error::MatcError`] if the type is not a struct, the field does not exist,
    /// or a private field on an external module struct is accessed.
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

    /// Synthesizes array and vector indexing expressions (`coll[idx]`).
    ///
    /// # Arguments
    ///
    /// * `expr` - Collection expression.
    /// * `index` - Integer index expression.
    /// * `span` - Source span of indexing.
    /// * `symbols` - Active symbol table.
    ///
    /// # Errors
    ///
    /// Returns [`crate::error::MatcError`] if the index is not integer or `expr` is not indexable.
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
            Type::Array(elem_ty, _) | Type::Vector(elem_ty) => Ok(*elem_ty),
            other => Err(self.type_error(format!("Cannot index non-array type {:?}", other), span)),
        }
    }

    /// Synthesizes tuple indexing expressions (`tuple.0`).
    ///
    /// # Arguments
    ///
    /// * `expr` - Tuple expression.
    /// * `index` - Zero-based positional index.
    /// * `span` - Source span of access.
    /// * `symbols` - Active symbol table.
    ///
    /// # Errors
    ///
    /// Returns [`crate::error::MatcError`] if `expr` is not a tuple or the index is out of bounds.
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
