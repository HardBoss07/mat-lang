use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::BinaryOp;
use crate::error::{MatcError, Result};
use inkwell::values::{AsValueRef, BasicValueEnum};
use inkwell::{FloatPredicate, IntPredicate};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_float_binary(
        &self,
        op: &BinaryOp,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let left_float = left_val.into_float_value();
        let right_float = right_val.into_float_value();
        let result: BasicValueEnum<'ctx> = match op {
            BinaryOp::Add => llvm_err(
                self.engine
                    .builder
                    .build_float_add(left_float, right_float, "addtmp"),
            )?
            .into(),
            BinaryOp::Sub => llvm_err(
                self.engine
                    .builder
                    .build_float_sub(left_float, right_float, "subtmp"),
            )?
            .into(),
            BinaryOp::Mul => llvm_err(
                self.engine
                    .builder
                    .build_float_mul(left_float, right_float, "multmp"),
            )?
            .into(),
            BinaryOp::Div => llvm_err(
                self.engine
                    .builder
                    .build_float_div(left_float, right_float, "divtmp"),
            )?
            .into(),
            BinaryOp::Mod => llvm_err(
                self.engine
                    .builder
                    .build_float_rem(left_float, right_float, "modtmp"),
            )?
            .into(),
            BinaryOp::Eq => llvm_err(self.engine.builder.build_float_compare(
                FloatPredicate::OEQ,
                left_float,
                right_float,
                "eqtmp",
            ))?
            .into(),
            BinaryOp::Neq => llvm_err(self.engine.builder.build_float_compare(
                FloatPredicate::ONE,
                left_float,
                right_float,
                "netmp",
            ))?
            .into(),
            BinaryOp::Lt => llvm_err(self.engine.builder.build_float_compare(
                FloatPredicate::OLT,
                left_float,
                right_float,
                "lttmp",
            ))?
            .into(),
            BinaryOp::Lte => llvm_err(self.engine.builder.build_float_compare(
                FloatPredicate::OLE,
                left_float,
                right_float,
                "ltetmp",
            ))?
            .into(),
            BinaryOp::Gt => llvm_err(self.engine.builder.build_float_compare(
                FloatPredicate::OGT,
                left_float,
                right_float,
                "gttmp",
            ))?
            .into(),
            BinaryOp::Gte => llvm_err(self.engine.builder.build_float_compare(
                FloatPredicate::OGE,
                left_float,
                right_float,
                "gtetmp",
            ))?
            .into(),
            _ => {
                return Err(MatcError::CodegenError(
                    "Unsupported operator for floats".to_string(),
                ));
            }
        };
        Ok(result)
    }

    pub(crate) fn compile_string_binary(
        &self,
        op: &BinaryOp,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let left_ptr = left_val.into_pointer_value();
        let right_ptr = right_val.into_pointer_value();

        let strcmp_fn = self
            .engine
            .module
            .get_function("strcmp")
            .unwrap_or_else(|| {
                let ptr_type = self
                    .engine
                    .context
                    .ptr_type(inkwell::AddressSpace::default());
                let fn_type = self
                    .engine
                    .context
                    .i32_type()
                    .fn_type(&[ptr_type.into(), ptr_type.into()], false);
                self.engine.module.add_function("strcmp", fn_type, None)
            });

        let call_result = llvm_err(self.engine.builder.build_call(
            strcmp_fn,
            &[left_ptr.into(), right_ptr.into()],
            "strcmp_call",
        ))?;
        let cmp_int = unsafe { BasicValueEnum::new(call_result.as_value_ref()) }.into_int_value();
        let zero = self.engine.context.i32_type().const_int(0, false);

        let result: BasicValueEnum<'ctx> = match op {
            BinaryOp::Eq => llvm_err(self.engine.builder.build_int_compare(
                IntPredicate::EQ,
                cmp_int,
                zero,
                "streq",
            ))?
            .into(),
            BinaryOp::Neq => llvm_err(self.engine.builder.build_int_compare(
                IntPredicate::NE,
                cmp_int,
                zero,
                "strneq",
            ))?
            .into(),
            _ => {
                return Err(MatcError::CodegenError(
                    "Unsupported operator for string pointers".to_string(),
                ));
            }
        };
        Ok(result)
    }
}
