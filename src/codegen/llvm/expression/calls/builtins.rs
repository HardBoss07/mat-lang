//! Built-in runtime function call code generation.
//!
//! This module compiles calls to language built-ins like `println` and `Vec::new`.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::{Expression, Type};
use crate::error::{MatcError, Result};
use inkwell::types::BasicType;
use inkwell::values::{AsValueRef, BasicMetadataValueEnum, BasicValueEnum};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles a call to the built-in `println` runtime routine.
    pub(crate) fn compile_builtin_println(
        &mut self,
        arguments: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        if let Some(first_arg) = arguments.first() {
            let val = self.compile_expression(first_arg)?;
            self.emit_typed_println_call(val)?;
        }
        Ok(self.engine.context.i32_type().const_int(0, false).into())
    }

    /// Emits type-specific dispatch for println based on LLVM value type.
    fn emit_typed_println_call(&mut self, val: BasicValueEnum<'ctx>) -> Result<()> {
        if val.is_pointer_value() {
            let fn_val = self
                .engine
                .module
                .get_function("_mat_rt_println_str")
                .unwrap();
            llvm_err(
                self.engine
                    .builder
                    .build_call(fn_val, &[val.into()], "call_println_str"),
            )?;
        } else if val.is_int_value() {
            self.emit_int_println_call(val.into_int_value())?;
        } else if val.is_float_value() {
            let fn_val = self
                .engine
                .module
                .get_function("_mat_rt_println_float")
                .unwrap();
            llvm_err(
                self.engine
                    .builder
                    .build_call(fn_val, &[val.into()], "call_println_float"),
            )?;
        }
        Ok(())
    }

    /// Emits boolean or integer specific println dispatch.
    fn emit_int_println_call(&mut self, int_val: inkwell::values::IntValue<'ctx>) -> Result<()> {
        if int_val.get_type().get_bit_width() == 1 {
            let fn_val = self
                .engine
                .module
                .get_function("_mat_rt_println_bool")
                .unwrap();
            llvm_err(self.engine.builder.build_call(
                fn_val,
                &[int_val.into()],
                "call_println_bool",
            ))?;
        } else {
            let fn_val = self
                .engine
                .module
                .get_function("_mat_rt_println_int")
                .unwrap();
            let i64_val: BasicMetadataValueEnum<'ctx> = if int_val.get_type().get_bit_width() < 64 {
                llvm_err(self.engine.builder.build_int_s_extend(
                    int_val,
                    self.engine.context.i64_type(),
                    "i64_ext",
                ))?
                .into()
            } else {
                int_val.into()
            };
            llvm_err(
                self.engine
                    .builder
                    .build_call(fn_val, &[i64_val], "call_println_int"),
            )?;
        }
        Ok(())
    }

    /// Compiles a call to `Vec::new()`, creating a heap-allocated vector struct with exact element stride.
    pub(crate) fn compile_builtin_vec_new(
        &mut self,
        call_expr: &Expression,
        expected_elem_ty: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let elem_mat_ty = if let Some(target_elem) = expected_elem_ty {
            if *target_elem != Type::Infer {
                target_elem.clone()
            } else {
                let vec_mat_ty = self
                    .type_checker
                    .synthesize_expr(call_expr, &self.symbol_table)?;
                match &vec_mat_ty {
                    Type::Vector(elem) if **elem != Type::Infer => (**elem).clone(),
                    _ => Type::Int,
                }
            }
        } else {
            let vec_mat_ty = self
                .type_checker
                .synthesize_expr(call_expr, &self.symbol_table)?;
            match &vec_mat_ty {
                Type::Vector(elem) if **elem != Type::Infer => (**elem).clone(),
                _ => Type::Int,
            }
        };

        let elem_llvm_ty = self.engine.llvm_type(&elem_mat_ty);
        let elem_size = elem_llvm_ty.size_of().ok_or_else(|| {
            MatcError::CodegenError(
                "Cannot determine size of vector element type for Vec::new()".to_string(),
            )
        })?;

        let init_cap = self.engine.context.i64_type().const_int(4, false);
        let create_fn = self
            .engine
            .module
            .get_function("_mat_rt_vec_create")
            .unwrap();
        let call_site = llvm_err(self.engine.builder.build_call(
            create_fn,
            &[elem_size.into(), init_cap.into()],
            "call_vec_new",
        ))?;
        Ok(unsafe { BasicValueEnum::new(call_site.as_value_ref()) })
    }
}
