mod enum_match;
mod result_match;

use super::super::function::FunctionCompiler;
use crate::ast::{Expression, MatchArm, Type};
use crate::error::Result;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
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
