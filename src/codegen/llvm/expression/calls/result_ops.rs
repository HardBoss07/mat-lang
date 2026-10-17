//! Result type `Ok(...)` and `Err(...)` expression code generation.
//!
//! This module compiles Result variant constructors into LLVM struct allocations and stores.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::{Expression, Type};
use crate::error::Result;
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles an `Ok(...)` expression into a Result-typed LLVM struct value.
    ///
    /// # Arguments
    ///
    /// * `val_expr` - The inner payload expression.
    ///
    /// # Returns
    ///
    /// The loaded Result struct value.
    ///
    /// # Errors
    ///
    /// Returns a [`crate::error::MatcError`] if LLVM construction fails.
    pub(crate) fn compile_ok_expr(
        &mut self,
        val_expr: &Expression,
    ) -> Result<BasicValueEnum<'ctx>> {
        let inner_val = self.compile_expression(val_expr)?;
        let ok_ty = self
            .type_checker
            .synthesize_expr(val_expr, &self.symbol_table)
            .unwrap_or(Type::Int);
        let result_mat_ty = Type::Result(Box::new(ok_ty), Box::new(Type::String));
        let result_llvm_ty = self.engine.llvm_type(&result_mat_ty);

        let alloca = self.create_entry_block_alloca(result_llvm_ty, "ok_tmp")?;

        let tag_ptr = llvm_err(self.engine.builder.build_struct_gep(
            result_llvm_ty.into_struct_type(),
            alloca,
            0,
            "tag_ptr",
        ))?;
        let tru_val = self.engine.context.bool_type().const_int(1, false);
        llvm_err(self.engine.builder.build_store(tag_ptr, tru_val))?;

        let val_ptr = llvm_err(self.engine.builder.build_struct_gep(
            result_llvm_ty.into_struct_type(),
            alloca,
            1,
            "val_ptr",
        ))?;
        llvm_err(self.engine.builder.build_store(val_ptr, inner_val))?;

        let loaded = llvm_err(
            self.engine
                .builder
                .build_load(result_llvm_ty, alloca, "ok_struct"),
        )?;
        Ok(loaded)
    }

    /// Compiles an `Err(...)` expression into a Result-typed LLVM struct value.
    ///
    /// # Arguments
    ///
    /// * `err_expr` - The inner error payload expression.
    ///
    /// # Returns
    ///
    /// The loaded Result struct value.
    ///
    /// # Errors
    ///
    /// Returns a [`crate::error::MatcError`] if LLVM construction fails.
    pub(crate) fn compile_err_expr(
        &mut self,
        err_expr: &Expression,
    ) -> Result<BasicValueEnum<'ctx>> {
        let inner_err = self.compile_expression(err_expr)?;
        let err_ty = self
            .type_checker
            .synthesize_expr(err_expr, &self.symbol_table)
            .unwrap_or(Type::String);
        let result_mat_ty = Type::Result(Box::new(Type::Int), Box::new(err_ty));
        let result_llvm_ty = self.engine.llvm_type(&result_mat_ty);

        let alloca = self.create_entry_block_alloca(result_llvm_ty, "err_tmp")?;

        let tag_ptr = llvm_err(self.engine.builder.build_struct_gep(
            result_llvm_ty.into_struct_type(),
            alloca,
            0,
            "tag_ptr",
        ))?;
        let fal_val = self.engine.context.bool_type().const_int(0, false);
        llvm_err(self.engine.builder.build_store(tag_ptr, fal_val))?;

        let err_ptr = llvm_err(self.engine.builder.build_struct_gep(
            result_llvm_ty.into_struct_type(),
            alloca,
            2,
            "err_ptr",
        ))?;
        llvm_err(self.engine.builder.build_store(err_ptr, inner_err))?;

        let loaded = llvm_err(self.engine.builder.build_load(
            result_llvm_ty,
            alloca,
            "err_struct",
        ))?;
        Ok(loaded)
    }
}
