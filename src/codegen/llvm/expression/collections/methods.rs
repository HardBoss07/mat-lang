//! Method call code generation for collections and containers.
//!
//! This module compiles method calls on vectors, arrays, and tuples such as `push`, `pop`, `get`, and `len`.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::Expression;
use crate::error::{MatcError, Result};
use inkwell::values::{AsValueRef, BasicValueEnum};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles a method invocation on a collection or container receiver.
    ///
    /// # Arguments
    ///
    /// * `receiver` - Receiver expression on which the method is invoked.
    /// * `method` - Method name string.
    /// * `args` - Argument expressions.
    ///
    /// # Returns
    ///
    /// The return value of the method call.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if the method is unsupported or building fails.
    pub(crate) fn compile_method_call(
        &mut self,
        receiver: &Expression,
        method: &str,
        args: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        let receiver_mat_ty = self
            .type_checker
            .synthesize_expr(receiver, &self.symbol_table)?;

        match (&receiver_mat_ty, method) {
            (crate::ast::Type::Vector(elem_mat_ty), "push") => {
                self.compile_vector_push_method(receiver, elem_mat_ty, args)
            }
            (crate::ast::Type::Vector(elem_mat_ty), "pop") => {
                self.compile_vector_pop_method(receiver, elem_mat_ty)
            }
            (crate::ast::Type::Vector(elem_mat_ty), "get") => {
                self.compile_vector_get_method(receiver, elem_mat_ty, args)
            }
            (crate::ast::Type::Vector(_), "len") => self.compile_vector_len_method(receiver),
            (crate::ast::Type::Array(_, len), "len") => Ok(self
                .engine
                .context
                .i64_type()
                .const_int(*len as u64, false)
                .into()),
            (crate::ast::Type::Tuple(fields), "len") => Ok(self
                .engine
                .context
                .i64_type()
                .const_int(fields.len() as u64, false)
                .into()),
            _ => Err(MatcError::CodegenError(format!(
                "Unsupported method '{}' on {:?}",
                method, receiver_mat_ty
            ))),
        }
    }

    /// Compiles vector `push` method invocation.
    fn compile_vector_push_method(
        &mut self,
        receiver: &Expression,
        elem_mat_ty: &crate::ast::Type,
        args: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        let vec_val = self.compile_expression(receiver)?;
        let elem_llvm_ty = self.engine.llvm_type(elem_mat_ty);
        let push_fn = self.engine.module.get_function("_mat_rt_vec_push").unwrap();

        for arg in args {
            let val = self.compile_expression(arg)?;
            let coerced =
                super::super::super::util::coerce_val_to_type(self.engine, val, elem_mat_ty)?;
            let arg_alloca = self.create_entry_block_alloca(elem_llvm_ty, "push_arg")?;
            llvm_err(self.engine.builder.build_store(arg_alloca, coerced))?;
            llvm_err(self.engine.builder.build_call(
                push_fn,
                &[vec_val.into(), arg_alloca.into()],
                "call_push",
            ))?;
        }
        Ok(self.engine.context.i32_type().const_int(0, false).into())
    }

    /// Compiles vector `pop` method invocation.
    fn compile_vector_pop_method(
        &mut self,
        receiver: &Expression,
        elem_mat_ty: &crate::ast::Type,
    ) -> Result<BasicValueEnum<'ctx>> {
        let vec_val = self.compile_expression(receiver)?;
        let elem_llvm_ty = self.engine.llvm_type(elem_mat_ty);
        let out_alloca = self.create_entry_block_alloca(elem_llvm_ty, "pop_out")?;

        let pop_fn = self.engine.module.get_function("_mat_rt_vec_pop").unwrap();
        llvm_err(self.engine.builder.build_call(
            pop_fn,
            &[vec_val.into(), out_alloca.into()],
            "call_pop",
        ))?;

        let loaded = llvm_err(
            self.engine
                .builder
                .build_load(elem_llvm_ty, out_alloca, "pop_val"),
        )?;
        Ok(loaded)
    }

    /// Compiles vector `get` method invocation.
    fn compile_vector_get_method(
        &mut self,
        receiver: &Expression,
        elem_mat_ty: &crate::ast::Type,
        args: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        let vec_val = self.compile_expression(receiver)?;
        let elem_llvm_ty = self.engine.llvm_type(elem_mat_ty);
        let index_val = self.compile_expression(&args[0])?.into_int_value();
        let i64_index = if index_val.get_type().get_bit_width() < 64 {
            llvm_err(self.engine.builder.build_int_s_extend(
                index_val,
                self.engine.context.i64_type(),
                "idx_ext",
            ))?
        } else {
            index_val
        };

        let get_fn = self.engine.module.get_function("_mat_rt_vec_get").unwrap();
        let call_res = llvm_err(self.engine.builder.build_call(
            get_fn,
            &[vec_val.into(), i64_index.into()],
            "call_vec_get",
        ))?;
        let elem_ptr = unsafe { BasicValueEnum::new(call_res.as_value_ref()) }.into_pointer_value();
        let loaded = llvm_err(self.engine.builder.build_load(
            elem_llvm_ty,
            elem_ptr,
            "vec_elem_val",
        ))?;
        Ok(loaded)
    }

    /// Compiles vector `len` method invocation.
    fn compile_vector_len_method(&mut self, receiver: &Expression) -> Result<BasicValueEnum<'ctx>> {
        let vec_val = self.compile_expression(receiver)?;
        let len_fn = self.engine.module.get_function("_mat_rt_vec_len").unwrap();
        let call_res = llvm_err(self.engine.builder.build_call(
            len_fn,
            &[vec_val.into()],
            "call_vec_len",
        ))?;
        Ok(unsafe { BasicValueEnum::new(call_res.as_value_ref()) })
    }
}
