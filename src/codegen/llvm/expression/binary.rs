use super::super::function::FunctionCompiler;
use super::super::util::llvm_err;
use crate::ast::BinaryOp;
use crate::error::{MatcError, Result};
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_binary_expr(
        &mut self,
        op: &BinaryOp,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        if left_val.is_int_value() && right_val.is_int_value() {
            let l_int = left_val.into_int_value();
            let r_int = right_val.into_int_value();
            let res: BasicValueEnum<'ctx> = match op {
                BinaryOp::Add => {
                    llvm_err(self.engine.builder.build_int_add(l_int, r_int, "addtmp"))?.into()
                }
                BinaryOp::Sub => {
                    llvm_err(self.engine.builder.build_int_sub(l_int, r_int, "subtmp"))?.into()
                }
                BinaryOp::Mul => {
                    llvm_err(self.engine.builder.build_int_mul(l_int, r_int, "multmp"))?.into()
                }
                BinaryOp::Div => llvm_err(
                    self.engine
                        .builder
                        .build_int_signed_div(l_int, r_int, "divtmp"),
                )?
                .into(),
                BinaryOp::Mod => llvm_err(
                    self.engine
                        .builder
                        .build_int_signed_rem(l_int, r_int, "modtmp"),
                )?
                .into(),
                BinaryOp::Shl => {
                    llvm_err(self.engine.builder.build_left_shift(l_int, r_int, "shltmp"))?.into()
                }
                BinaryOp::Shr => llvm_err(
                    self.engine
                        .builder
                        .build_right_shift(l_int, r_int, true, "shrtmp"),
                )?
                .into(),
                BinaryOp::Eq => llvm_err(self.engine.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    l_int,
                    r_int,
                    "eqtmp",
                ))?
                .into(),
                BinaryOp::Neq => llvm_err(self.engine.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    l_int,
                    r_int,
                    "netmp",
                ))?
                .into(),
                BinaryOp::Lt => llvm_err(self.engine.builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    l_int,
                    r_int,
                    "lttmp",
                ))?
                .into(),
                BinaryOp::Lte => llvm_err(self.engine.builder.build_int_compare(
                    inkwell::IntPredicate::SLE,
                    l_int,
                    r_int,
                    "ltetmp",
                ))?
                .into(),
                BinaryOp::Gt => llvm_err(self.engine.builder.build_int_compare(
                    inkwell::IntPredicate::SGT,
                    l_int,
                    r_int,
                    "gttmp",
                ))?
                .into(),
                BinaryOp::Gte => llvm_err(self.engine.builder.build_int_compare(
                    inkwell::IntPredicate::SGE,
                    l_int,
                    r_int,
                    "gtetmp",
                ))?
                .into(),
                BinaryOp::And => {
                    llvm_err(self.engine.builder.build_and(l_int, r_int, "andtmp"))?.into()
                }
                BinaryOp::Or => {
                    llvm_err(self.engine.builder.build_or(l_int, r_int, "ortmp"))?.into()
                }
            };
            Ok(res)
        } else if left_val.is_float_value() && right_val.is_float_value() {
            let l_float = left_val.into_float_value();
            let r_float = right_val.into_float_value();
            let res: BasicValueEnum<'ctx> = match op {
                BinaryOp::Add => llvm_err(
                    self.engine
                        .builder
                        .build_float_add(l_float, r_float, "addtmp"),
                )?
                .into(),
                BinaryOp::Sub => llvm_err(
                    self.engine
                        .builder
                        .build_float_sub(l_float, r_float, "subtmp"),
                )?
                .into(),
                BinaryOp::Mul => llvm_err(
                    self.engine
                        .builder
                        .build_float_mul(l_float, r_float, "multmp"),
                )?
                .into(),
                BinaryOp::Div => llvm_err(
                    self.engine
                        .builder
                        .build_float_div(l_float, r_float, "divtmp"),
                )?
                .into(),
                BinaryOp::Mod => llvm_err(
                    self.engine
                        .builder
                        .build_float_rem(l_float, r_float, "modtmp"),
                )?
                .into(),
                _ => {
                    return Err(MatcError::CodegenError(
                        "Unsupported operator for floats".to_string(),
                    ));
                }
            };
            Ok(res)
        } else {
            Err(MatcError::CodegenError(
                "Mismatched or unsupported types in binary operation".to_string(),
            ))
        }
    }
}
