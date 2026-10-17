use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::BinaryOp;
use crate::error::Result;
use inkwell::values::BasicValueEnum;
use inkwell::IntPredicate;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_int_binary(
        &self,
        op: &BinaryOp,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let left_int = left_val.into_int_value();
        let right_int = right_val.into_int_value();
        let result: BasicValueEnum<'ctx> = match op {
            BinaryOp::Add => {
                llvm_err(self.engine.builder.build_int_add(left_int, right_int, "addtmp"))?.into()
            }
            BinaryOp::Sub => {
                llvm_err(self.engine.builder.build_int_sub(left_int, right_int, "subtmp"))?.into()
            }
            BinaryOp::Mul => {
                llvm_err(self.engine.builder.build_int_mul(left_int, right_int, "multmp"))?.into()
            }
            BinaryOp::Div => llvm_err(
                self.engine
                    .builder
                    .build_int_signed_div(left_int, right_int, "divtmp"),
            )?
            .into(),
            BinaryOp::Mod => llvm_err(
                self.engine
                    .builder
                    .build_int_signed_rem(left_int, right_int, "modtmp"),
            )?
            .into(),
            BinaryOp::Shl => {
                llvm_err(self.engine.builder.build_left_shift(left_int, right_int, "shltmp"))?
                    .into()
            }
            BinaryOp::Shr => llvm_err(
                self.engine
                    .builder
                    .build_right_shift(left_int, right_int, true, "shrtmp"),
            )?
            .into(),
            BinaryOp::Eq => llvm_err(self.engine.builder.build_int_compare(
                IntPredicate::EQ,
                left_int,
                right_int,
                "eqtmp",
            ))?
            .into(),
            BinaryOp::Neq => llvm_err(self.engine.builder.build_int_compare(
                IntPredicate::NE,
                left_int,
                right_int,
                "netmp",
            ))?
            .into(),
            BinaryOp::Lt => llvm_err(self.engine.builder.build_int_compare(
                IntPredicate::SLT,
                left_int,
                right_int,
                "lttmp",
            ))?
            .into(),
            BinaryOp::Lte => llvm_err(self.engine.builder.build_int_compare(
                IntPredicate::SLE,
                left_int,
                right_int,
                "ltetmp",
            ))?
            .into(),
            BinaryOp::Gt => llvm_err(self.engine.builder.build_int_compare(
                IntPredicate::SGT,
                left_int,
                right_int,
                "gttmp",
            ))?
            .into(),
            BinaryOp::Gte => llvm_err(self.engine.builder.build_int_compare(
                IntPredicate::SGE,
                left_int,
                right_int,
                "gtetmp",
            ))?
            .into(),
            BinaryOp::And => {
                llvm_err(self.engine.builder.build_and(left_int, right_int, "andtmp"))?.into()
            }
            BinaryOp::Or => {
                llvm_err(self.engine.builder.build_or(left_int, right_int, "ortmp"))?.into()
            }
        };
        Ok(result)
    }
}
