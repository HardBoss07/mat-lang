//! Intrinsic and runtime function call type synthesis.
//!
//! Handles built-in console routines (`println`, `print`) and low-level runtime calls (`_mat_rt_*`).

use crate::ast::{Expression, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Synthesizes print/println built-in calls by verifying argument expressions and returning `Type::Void`.
    ///
    /// # Arguments
    ///
    /// * `arguments` - List of arguments passed to print/println.
    /// * `symbols` - Active symbol table.
    pub(crate) fn synthesize_print_call(
        &self,
        arguments: &[Expression],
        symbols: &SymbolTable,
    ) -> Result<Type> {
        for argument in arguments {
            self.synthesize_expr(argument, symbols)?;
        }
        Ok(Type::Void)
    }

    /// Synthesizes MAT runtime library calls based on intrinsic name semantics.
    ///
    /// # Arguments
    ///
    /// * `callee` - Intrinsic function identifier starting with `_mat_rt_`.
    /// * `arguments` - Runtime function arguments.
    /// * `symbols` - Active symbol table.
    pub(crate) fn synthesize_runtime_call(
        &self,
        callee: &str,
        arguments: &[Expression],
        symbols: &SymbolTable,
    ) -> Result<Type> {
        for argument in arguments {
            self.synthesize_expr(argument, symbols)?;
        }

        let ret_type = if callee.contains("read_line") || callee.contains("read_to_string") {
            Type::String
        } else if callee.contains("open")
            || callee.contains("write")
            || callee.contains("seek")
            || callee.contains("abs_int")
        {
            Type::Int
        } else if callee.contains("flush") || callee.contains("close") {
            Type::Bool
        } else if callee.contains("print") {
            Type::Void
        } else {
            Type::F64
        };

        Ok(ret_type)
    }
}
