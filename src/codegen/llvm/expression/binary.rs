//! Binary expression code generation.
//!
//! This module coordinates compiling binary arithmetic, bitwise, logic,
//! comparison, and string equality operations into LLVM IR.

pub mod coercion;
pub mod float_string_ops;
pub mod int_ops;

#[allow(unused_imports)]
pub(crate) use coercion::*;
#[allow(unused_imports)]
pub(crate) use float_string_ops::*;
#[allow(unused_imports)]
pub(crate) use int_ops::*;

use super::super::function::FunctionCompiler;
use crate::ast::BinaryOp;
use crate::error::{MatcError, Result};
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles a binary operation between two evaluated operands.
    ///
    /// Coerces operands when necessary (e.g., integer width widening or int-to-float promotion)
    /// and delegates to specialized handlers based on the operand types:
    /// - Integer / Boolean operations -> [`Self::compile_int_binary`]
    /// - Floating-point operations -> [`Self::compile_float_binary`]
    /// - String pointer operations -> [`Self::compile_string_binary`]
    ///
    /// # Arguments
    ///
    /// * `op` - The binary operator being applied.
    /// * `left_val` - The left operand's evaluated LLVM value.
    /// * `right_val` - The right operand's evaluated LLVM value.
    ///
    /// # Returns
    ///
    /// The resulting LLVM [`BasicValueEnum`].
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError::CodegenError`] if operand types are incompatible or unsupported.
    pub(crate) fn compile_binary_expr(
        &mut self,
        op: &BinaryOp,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let (left_val, right_val) = self.coerce_binary_operands(left_val, right_val)?;

        if left_val.is_int_value() && right_val.is_int_value() {
            self.compile_int_binary(op, left_val, right_val)
        } else if left_val.is_float_value() && right_val.is_float_value() {
            self.compile_float_binary(op, left_val, right_val)
        } else if left_val.is_pointer_value() && right_val.is_pointer_value() {
            self.compile_string_binary(op, left_val, right_val)
        } else {
            Err(MatcError::CodegenError(
                "Mismatched or unsupported types in binary operation".to_string(),
            ))
        }
    }
}
