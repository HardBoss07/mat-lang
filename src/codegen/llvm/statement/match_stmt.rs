//! Match statement compilation dispatch.
//!
//! Routes match expressions to the appropriate handler based on the
//! matched expression's type: result matches for `Result<Ok, Err>` types
//! and enum matches for custom/struct nominal types.

mod enum_match;
mod result_match;

use super::super::function::FunctionCompiler;
use crate::ast::{Expression, MatchArm, Type};
use crate::error::Result;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles a match statement by dispatching to the correct handler.
    ///
    /// Synthesizes the matched expression's type, compiles it to an LLVM value,
    /// then delegates to [`compile_result_match`](Self::compile_result_match) for
    /// `Result` types or [`compile_enum_match`](Self::compile_enum_match) for
    /// enum/struct types.
    pub(crate) fn compile_match_stmt(
        &mut self,
        expr: &Expression,
        arms: &[MatchArm],
    ) -> Result<()> {
        let expr_mat_ty = self
            .type_checker
            .synthesize_expr(expr, &self.symbol_table)?;
        let compiled_expr = self.compile_expression(expr)?;

        let match_after = self
            .engine
            .context
            .append_basic_block(self.fn_value, "match_after");

        match &expr_mat_ty {
            Type::Result(ok_ty, err_ty) => {
                self.compile_result_match(
                    ok_ty,
                    err_ty,
                    &expr_mat_ty,
                    compiled_expr,
                    arms,
                    match_after,
                )?;
            }
            Type::Custom(enum_name) | Type::Struct(enum_name) => {
                self.compile_enum_match(enum_name, &expr_mat_ty, compiled_expr, arms, match_after)?;
            }
            _ => {}
        }
        Ok(())
    }
}
