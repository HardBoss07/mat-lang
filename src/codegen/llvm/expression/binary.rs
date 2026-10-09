use super::super::function::FunctionCompiler;
use super::super::util::llvm_err;
use crate::ast::BinaryOp;
use crate::error::{MatcError, Result};
use inkwell::values::{AsValueRef, BasicValueEnum};
use inkwell::{FloatPredicate, IntPredicate};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_binary_expr(
        &mut self,
        op: &BinaryOp,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let (left_val, right_val) = match (left_val, right_val) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                let l_width = l.get_type().get_bit_width();
                let r_width = r.get_type().get_bit_width();
                if l_width < r_width {
                    let ext = llvm_err(self.engine.builder.build_int_s_extend(
                        l,
                        r.get_type(),
                        "l_ext",
                    ))?;
                    (ext.into(), r.into())
                } else if r_width < l_width {
                    let ext = llvm_err(self.engine.builder.build_int_s_extend(
                        r,
                        l.get_type(),
                        "r_ext",
                    ))?;
                    (l.into(), ext.into())
                } else {
                    (l.into(), r.into())
                }
            }
            (BasicValueEnum::IntValue(l), BasicValueEnum::FloatValue(r)) => {
                let conv = llvm_err(self.engine.builder.build_signed_int_to_float(
                    l,
                    r.get_type(),
                    "int_to_float",
                ))?;
                (conv.into(), r.into())
            }
            (BasicValueEnum::FloatValue(l), BasicValueEnum::IntValue(r)) => {
                let conv = llvm_err(self.engine.builder.build_signed_int_to_float(
                    r,
                    l.get_type(),
                    "int_to_float",
                ))?;
                (l.into(), conv.into())
            }
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                if l.get_type() != r.get_type() {
                    let ext = llvm_err(self.engine.builder.build_float_ext(
                        l,
                        self.engine.context.f64_type(),
                        "f_ext",
                    ))?;
                    (ext.into(), r.into())
                } else {
                    (l.into(), r.into())
                }
            }
            _ => (left_val, right_val),
        };

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
                    IntPredicate::EQ,
                    l_int,
                    r_int,
                    "eqtmp",
                ))?
                .into(),
                BinaryOp::Neq => llvm_err(self.engine.builder.build_int_compare(
                    IntPredicate::NE,
                    l_int,
                    r_int,
                    "netmp",
                ))?
                .into(),
                BinaryOp::Lt => llvm_err(self.engine.builder.build_int_compare(
                    IntPredicate::SLT,
                    l_int,
                    r_int,
                    "lttmp",
                ))?
                .into(),
                BinaryOp::Lte => llvm_err(self.engine.builder.build_int_compare(
                    IntPredicate::SLE,
                    l_int,
                    r_int,
                    "ltetmp",
                ))?
                .into(),
                BinaryOp::Gt => llvm_err(self.engine.builder.build_int_compare(
                    IntPredicate::SGT,
                    l_int,
                    r_int,
                    "gttmp",
                ))?
                .into(),
                BinaryOp::Gte => llvm_err(self.engine.builder.build_int_compare(
                    IntPredicate::SGE,
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
                BinaryOp::Eq => llvm_err(self.engine.builder.build_float_compare(
                    FloatPredicate::OEQ,
                    l_float,
                    r_float,
                    "eqtmp",
                ))?
                .into(),
                BinaryOp::Neq => llvm_err(self.engine.builder.build_float_compare(
                    FloatPredicate::ONE,
                    l_float,
                    r_float,
                    "netmp",
                ))?
                .into(),
                BinaryOp::Lt => llvm_err(self.engine.builder.build_float_compare(
                    FloatPredicate::OLT,
                    l_float,
                    r_float,
                    "lttmp",
                ))?
                .into(),
                BinaryOp::Lte => llvm_err(self.engine.builder.build_float_compare(
                    FloatPredicate::OLE,
                    l_float,
                    r_float,
                    "ltetmp",
                ))?
                .into(),
                BinaryOp::Gt => llvm_err(self.engine.builder.build_float_compare(
                    FloatPredicate::OGT,
                    l_float,
                    r_float,
                    "gttmp",
                ))?
                .into(),
                BinaryOp::Gte => llvm_err(self.engine.builder.build_float_compare(
                    FloatPredicate::OGE,
                    l_float,
                    r_float,
                    "gtetmp",
                ))?
                .into(),
                _ => {
                    return Err(MatcError::CodegenError(
                        "Unsupported operator for floats".to_string(),
                    ));
                }
            };
            Ok(res)
        } else if left_val.is_pointer_value() && right_val.is_pointer_value() {
            let l_ptr = left_val.into_pointer_value();
            let r_ptr = right_val.into_pointer_value();

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

            let call_res = llvm_err(self.engine.builder.build_call(
                strcmp_fn,
                &[l_ptr.into(), r_ptr.into()],
                "strcmp_call",
            ))?;
            let cmp_int = unsafe { BasicValueEnum::new(call_res.as_value_ref()) }.into_int_value();
            let zero = self.engine.context.i32_type().const_int(0, false);

            let res: BasicValueEnum<'ctx> = match op {
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
            Ok(res)
        } else {
            Err(MatcError::CodegenError(
                "Mismatched or unsupported types in binary operation".to_string(),
            ))
        }
    }
}
