use super::super::function::FunctionCompiler;
use super::super::util::{branch_if_unterminated, llvm_err};
use crate::ast::{Expression, MatchArm, MatchPattern, Type};
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

        if let Type::Result(ref ok_mat_ty, ref err_mat_ty) = expr_mat_ty {
            let result_llvm_ty = self.engine.llvm_type(&expr_mat_ty);
            let struct_ty = result_llvm_ty.into_struct_type();

            let alloca = self.create_entry_block_alloca(struct_ty.into(), "match_result_tmp")?;
            llvm_err(self.engine.builder.build_store(alloca, compiled_expr))?;

            let tag_ptr = llvm_err(
                self.engine
                    .builder
                    .build_struct_gep(struct_ty, alloca, 0, "tag_ptr"),
            )?;
            let tag_val = llvm_err(self.engine.builder.build_load(
                self.engine.context.bool_type(),
                tag_ptr,
                "tag_val",
            ))?
            .into_int_value();

            let block_ok = self
                .engine
                .context
                .append_basic_block(self.fn_value, "match_ok");
            let block_err = self
                .engine
                .context
                .append_basic_block(self.fn_value, "match_err");

            llvm_err(
                self.engine
                    .builder
                    .build_conditional_branch(tag_val, block_ok, block_err),
            )?;

            for arm in arms {
                match &arm.pattern {
                    MatchPattern::Ok(var_name) => {
                        self.engine.builder.position_at_end(block_ok);
                        self.symbol_table.push_scope();

                        let ok_llvm_ty = self.engine.llvm_type(ok_mat_ty);
                        let ok_ptr = llvm_err(
                            self.engine
                                .builder
                                .build_struct_gep(struct_ty, alloca, 1, "ok_ptr"),
                        )?;
                        let ok_val =
                            llvm_err(self.engine.builder.build_load(ok_llvm_ty, ok_ptr, var_name))?;

                        let var_alloca = self.create_entry_block_alloca(ok_llvm_ty, var_name)?;
                        llvm_err(self.engine.builder.build_store(var_alloca, ok_val))?;

                        self.local_vars
                            .insert(var_name.clone(), (var_alloca, (**ok_mat_ty).clone()));
                        self.symbol_table
                            .insert(var_name.clone(), (**ok_mat_ty).clone(), false);

                        for stmt in &arm.body {
                            self.compile_statement(stmt)?;
                        }
                        self.symbol_table.pop_scope();
                        branch_if_unterminated(self.engine, match_after)?;
                    }
                    MatchPattern::Err(var_name) => {
                        self.engine.builder.position_at_end(block_err);
                        self.symbol_table.push_scope();

                        let err_llvm_ty = self.engine.llvm_type(err_mat_ty);
                        let err_ptr = llvm_err(
                            self.engine
                                .builder
                                .build_struct_gep(struct_ty, alloca, 2, "err_ptr"),
                        )?;
                        let err_val = llvm_err(self.engine.builder.build_load(
                            err_llvm_ty,
                            err_ptr,
                            var_name,
                        ))?;

                        let var_alloca = self.create_entry_block_alloca(err_llvm_ty, var_name)?;
                        llvm_err(self.engine.builder.build_store(var_alloca, err_val))?;

                        self.local_vars
                            .insert(var_name.clone(), (var_alloca, (**err_mat_ty).clone()));
                        self.symbol_table
                            .insert(var_name.clone(), (**err_mat_ty).clone(), false);

                        for stmt in &arm.body {
                            self.compile_statement(stmt)?;
                        }
                        self.symbol_table.pop_scope();
                        branch_if_unterminated(self.engine, match_after)?;
                    }
                    _ => {}
                }
            }

            self.engine.builder.position_at_end(match_after);
        }
        Ok(())
    }
}
