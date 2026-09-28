use super::super::function::{FunctionCompiler, LoopBlocks};
use super::super::util::{branch_if_unterminated, llvm_err};
use crate::ast::{Expression, Statement, Type};
use crate::error::{MatcError, Result};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_fori_stmt(
        &mut self,
        init: &Statement,
        condition: &Expression,
        step: &Statement,
        body: &[Statement],
    ) -> Result<()> {
        self.symbol_table.push_scope();
        self.compile_statement(init)?;

        let for_cond = self
            .engine
            .context
            .append_basic_block(self.fn_value, "for_cond");
        let for_body = self
            .engine
            .context
            .append_basic_block(self.fn_value, "for_body");
        let for_step = self
            .engine
            .context
            .append_basic_block(self.fn_value, "for_step");
        let for_after = self
            .engine
            .context
            .append_basic_block(self.fn_value, "for_after");

        llvm_err(self.engine.builder.build_unconditional_branch(for_cond))?;

        self.engine.builder.position_at_end(for_cond);
        let cond_val = self.compile_expression(condition)?.into_int_value();
        llvm_err(
            self.engine
                .builder
                .build_conditional_branch(cond_val, for_body, for_after),
        )?;

        self.engine.builder.position_at_end(for_body);
        self.loop_stack.push(LoopBlocks {
            continue_target: for_step,
            break_target: for_after,
        });

        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.loop_stack.pop();

        branch_if_unterminated(self.engine, for_step)?;

        self.engine.builder.position_at_end(for_step);
        self.compile_statement(step)?;
        llvm_err(self.engine.builder.build_unconditional_branch(for_cond))?;

        self.engine.builder.position_at_end(for_after);
        self.symbol_table.pop_scope();
        Ok(())
    }

    pub(crate) fn compile_for_in_stmt(
        &mut self,
        var_name: &str,
        iterable: &Expression,
        body: &[Statement],
    ) -> Result<()> {
        let iter_mat_ty = self
            .type_checker
            .synthesize_expr(iterable, &self.symbol_table)?;
        let (elem_mat_ty, array_len) = match iter_mat_ty {
            Type::Array(ref elem, len) => (*elem.clone(), len),
            _ => {
                return Err(MatcError::CodegenError(
                    "Expected array type in for-in".to_string(),
                ));
            }
        };

        let iter_val = self.compile_expression(iterable)?;
        let array_llvm_ty = self.engine.llvm_type(&iter_mat_ty);
        let elem_llvm_ty = self.engine.llvm_type(&elem_mat_ty);
        let i64_ty = self.engine.context.i64_type();

        let array_alloca = llvm_err(
            self.engine
                .builder
                .build_alloca(array_llvm_ty, "for_in_arr"),
        )?;
        llvm_err(self.engine.builder.build_store(array_alloca, iter_val))?;

        let idx_alloca = llvm_err(self.engine.builder.build_alloca(i64_ty, "for_in_idx"))?;
        let zero_i64 = i64_ty.const_int(0, false);
        llvm_err(self.engine.builder.build_store(idx_alloca, zero_i64))?;

        let for_in_cond = self
            .engine
            .context
            .append_basic_block(self.fn_value, "for_in_cond");
        let for_in_body = self
            .engine
            .context
            .append_basic_block(self.fn_value, "for_in_body");
        let for_in_step = self
            .engine
            .context
            .append_basic_block(self.fn_value, "for_in_step");
        let for_in_after = self
            .engine
            .context
            .append_basic_block(self.fn_value, "for_in_after");

        llvm_err(self.engine.builder.build_unconditional_branch(for_in_cond))?;

        self.engine.builder.position_at_end(for_in_cond);
        let current_idx = llvm_err(
            self.engine
                .builder
                .build_load(i64_ty, idx_alloca, "curr_idx"),
        )?
        .into_int_value();
        let len_val = i64_ty.const_int(array_len as u64, false);
        let cond_val = llvm_err(self.engine.builder.build_int_compare(
            inkwell::IntPredicate::SLT,
            current_idx,
            len_val,
            "for_in_cmp",
        ))?;
        llvm_err(self.engine.builder.build_conditional_branch(
            cond_val,
            for_in_body,
            for_in_after,
        ))?;

        self.engine.builder.position_at_end(for_in_body);
        self.symbol_table.push_scope();

        let zero_i32 = self.engine.context.i32_type().const_int(0, false);
        let elem_ptr = unsafe {
            llvm_err(self.engine.builder.build_gep(
                array_llvm_ty,
                array_alloca,
                &[zero_i32, current_idx],
                "for_in_elem_ptr",
            ))?
        };
        let elem_val = llvm_err(
            self.engine
                .builder
                .build_load(elem_llvm_ty, elem_ptr, var_name),
        )?;

        let var_alloca = llvm_err(self.engine.builder.build_alloca(elem_llvm_ty, var_name))?;
        llvm_err(self.engine.builder.build_store(var_alloca, elem_val))?;

        self.local_vars
            .insert(var_name.to_string(), (var_alloca, elem_mat_ty.clone()));
        self.symbol_table
            .insert(var_name.to_string(), elem_mat_ty, false);

        self.loop_stack.push(LoopBlocks {
            continue_target: for_in_step,
            break_target: for_in_after,
        });

        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.loop_stack.pop();

        branch_if_unterminated(self.engine, for_in_step)?;

        self.symbol_table.pop_scope();

        self.engine.builder.position_at_end(for_in_step);
        let one_i64 = i64_ty.const_int(1, false);
        let next_idx = llvm_err(self.engine.builder.build_int_add(
            current_idx,
            one_i64,
            "next_idx",
        ))?;
        llvm_err(self.engine.builder.build_store(idx_alloca, next_idx))?;
        llvm_err(self.engine.builder.build_unconditional_branch(for_in_cond))?;

        self.engine.builder.position_at_end(for_in_after);
        Ok(())
    }
}
