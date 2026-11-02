//! Result match statement code generation.
//!
//! Compiles match expressions branching on `Ok`/`Err` result variants,
//! extracting the success or error payload into local scope variables
//! and generating the conditional LLVM IR branch structure.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::{branch_if_unterminated, llvm_err};
use crate::ast::{MatchArm, MatchPattern, Type};
use crate::error::Result;
use inkwell::basic_block::BasicBlock;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, PointerValue};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles a `match` statement over a `Result<Ok, Err>` value.
    ///
    /// Emits LLVM IR that stores the compiled result expression into a local
    /// alloca, extracts the boolean tag (index 0) to determine Ok vs Err,
    /// creates two basic blocks, and delegates compilation of each arm to
    /// [`compile_ok_arm`](Self::compile_ok_arm) or
    /// [`compile_err_arm`](Self::compile_err_arm).
    pub(crate) fn compile_result_match(
        &mut self,
        ok_mat_ty: &Type,
        err_mat_ty: &Type,
        expr_mat_ty: &Type,
        compiled_expr: BasicValueEnum<'ctx>,
        arms: &[MatchArm],
        match_after: BasicBlock<'ctx>,
    ) -> Result<()> {
        let result_llvm_ty = self.engine.llvm_type(expr_mat_ty);
        let struct_ty = result_llvm_ty.into_struct_type();

        let alloca = self.create_entry_block_alloca(struct_ty.into(), "match_result_tmp")?;
        llvm_err(self.engine.builder.build_store(alloca, compiled_expr))?;
        let tag_val = self.load_tag_i1(struct_ty, alloca)?;

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
                    self.compile_ok_arm(
                        var_name,
                        ok_mat_ty,
                        struct_ty,
                        alloca,
                        block_ok,
                        &arm.body,
                        match_after,
                    )?;
                }
                MatchPattern::Err(var_name) => {
                    self.compile_err_arm(
                        var_name,
                        err_mat_ty,
                        struct_ty,
                        alloca,
                        block_err,
                        &arm.body,
                        match_after,
                    )?;
                }
                _ => {}
            }
        }

        branch_if_unterminated(self.engine, match_after)?;
        self.engine.builder.position_at_end(match_after);
        Ok(())
    }

    /// Loads the boolean tag (field index 0) from a result struct alloca.
    ///
    /// The tag is an `i1` where `true` indicates `Ok` and `false` indicates `Err`.
    fn load_tag_i1(
        &self,
        struct_ty: StructType<'ctx>,
        alloca: PointerValue<'ctx>,
    ) -> Result<inkwell::values::IntValue<'ctx>> {
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
        Ok(tag_val)
    }

    /// Compiles the `Ok(var)` arm of a result match.
    ///
    /// Extracts the Ok payload value from struct field index 1, stores it in
    /// a new local variable binding, compiles the arm body, then branches to
    /// the merge block.
    fn compile_ok_arm(
        &mut self,
        var_name: &str,
        ok_mat_ty: &Type,
        struct_ty: StructType<'ctx>,
        alloca: PointerValue<'ctx>,
        block_ok: BasicBlock<'ctx>,
        body: &[crate::ast::Statement],
        match_after: BasicBlock<'ctx>,
    ) -> Result<()> {
        self.engine.builder.position_at_end(block_ok);
        let saved_vars = self.push_scope();

        let ok_llvm_ty = self.engine.llvm_type(ok_mat_ty);
        let ok_ptr = llvm_err(
            self.engine
                .builder
                .build_struct_gep(struct_ty, alloca, 1, "ok_ptr"),
        )?;
        let ok_val = llvm_err(self.engine.builder.build_load(ok_llvm_ty, ok_ptr, var_name))?;

        let var_alloca = self.create_entry_block_alloca(ok_llvm_ty, var_name)?;
        llvm_err(self.engine.builder.build_store(var_alloca, ok_val))?;

        self.local_vars
            .insert(var_name.to_string(), (var_alloca, ok_mat_ty.clone()));
        self.symbol_table
            .insert(var_name.to_string(), ok_mat_ty.clone(), false);

        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.pop_scope(saved_vars);
        branch_if_unterminated(self.engine, match_after)?;
        Ok(())
    }

    /// Compiles the `Err(var)` arm of a result match.
    ///
    /// Extracts the Err payload value from struct field index 2, stores it in
    /// a new local variable binding, compiles the arm body, then branches to
    /// the merge block.
    fn compile_err_arm(
        &mut self,
        var_name: &str,
        err_mat_ty: &Type,
        struct_ty: StructType<'ctx>,
        alloca: PointerValue<'ctx>,
        block_err: BasicBlock<'ctx>,
        body: &[crate::ast::Statement],
        match_after: BasicBlock<'ctx>,
    ) -> Result<()> {
        self.engine.builder.position_at_end(block_err);
        let saved_vars = self.push_scope();

        let err_llvm_ty = self.engine.llvm_type(err_mat_ty);
        let err_ptr = llvm_err(
            self.engine
                .builder
                .build_struct_gep(struct_ty, alloca, 2, "err_ptr"),
        )?;
        let err_val = llvm_err(
            self.engine
                .builder
                .build_load(err_llvm_ty, err_ptr, var_name),
        )?;

        let var_alloca = self.create_entry_block_alloca(err_llvm_ty, var_name)?;
        llvm_err(self.engine.builder.build_store(var_alloca, err_val))?;

        self.local_vars
            .insert(var_name.to_string(), (var_alloca, err_mat_ty.clone()));
        self.symbol_table
            .insert(var_name.to_string(), err_mat_ty.clone(), false);

        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.pop_scope(saved_vars);
        branch_if_unterminated(self.engine, match_after)?;
        Ok(())
    }
}
