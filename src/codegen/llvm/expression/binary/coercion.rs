//! Implicit operand type coercion for binary operations.
//!
//! Handles integer width widening, integer-to-float promotion, and float precision
//! extension prior to instruction generation.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::error::Result;
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Coerces binary operands to matching types according to Mat-Lang promotion semantics.
    ///
    /// - If both operands are integers of differing widths, sign-extends the narrower integer.
    /// - If one operand is an integer and the other is a float, converts the integer to float.
    /// - If both operands are floats of differing precision, extends to 64-bit float (`f64`).
    /// - Otherwise, returns operands unmodified.
    ///
    /// # Arguments
    ///
    /// * `left_val` - The left operand's evaluated LLVM value.
    /// * `right_val` - The right operand's evaluated LLVM value.
    ///
    /// # Returns
    ///
    /// A tuple `(coerced_left, coerced_right)` of matching LLVM values.
    ///
    /// # Errors
    ///
    /// Returns a [`crate::error::MatcError`] if LLVM cast instruction construction fails.
    pub(crate) fn coerce_binary_operands(
        &self,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
    ) -> Result<(BasicValueEnum<'ctx>, BasicValueEnum<'ctx>)> {
        let result = match (left_val, right_val) {
            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                let left_width = left.get_type().get_bit_width();
                let right_width = right.get_type().get_bit_width();
                if left_width < right_width {
                    let extended = llvm_err(self.engine.builder.build_int_s_extend(
                        left,
                        right.get_type(),
                        "l_ext",
                    ))?;
                    (extended.into(), right.into())
                } else if right_width < left_width {
                    let extended = llvm_err(self.engine.builder.build_int_s_extend(
                        right,
                        left.get_type(),
                        "r_ext",
                    ))?;
                    (left.into(), extended.into())
                } else {
                    (left.into(), right.into())
                }
            }
            (BasicValueEnum::IntValue(left), BasicValueEnum::FloatValue(right)) => {
                let converted = llvm_err(self.engine.builder.build_signed_int_to_float(
                    left,
                    right.get_type(),
                    "int_to_float",
                ))?;
                (converted.into(), right.into())
            }
            (BasicValueEnum::FloatValue(left), BasicValueEnum::IntValue(right)) => {
                let converted = llvm_err(self.engine.builder.build_signed_int_to_float(
                    right,
                    left.get_type(),
                    "int_to_float",
                ))?;
                (left.into(), converted.into())
            }
            (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                if left.get_type() != right.get_type() {
                    let extended = llvm_err(self.engine.builder.build_float_ext(
                        left,
                        self.engine.context.f64_type(),
                        "f_ext",
                    ))?;
                    (extended.into(), right.into())
                } else {
                    (left.into(), right.into())
                }
            }
            _ => (left_val, right_val),
        };
        Ok(result)
    }
}
