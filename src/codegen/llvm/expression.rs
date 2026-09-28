mod access;
mod binary;
mod calls;
mod collections;

use super::function::FunctionCompiler;
use super::util::llvm_err;
use crate::ast::Expression;
use crate::error::{MatcError, Result};
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub fn compile_expression(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expression::IntLiteral(val, _) => Ok(self
                .engine
                .context
                .i64_type()
                .const_int(*val as u64, true)
                .into()),
            Expression::FloatLiteral(val, _) => {
                Ok(self.engine.context.f64_type().const_float(*val).into())
            }
            Expression::BoolLiteral(val, _) => Ok(self
                .engine
                .context
                .bool_type()
                .const_int(*val as u64, false)
                .into()),
            Expression::StringLiteral(text, _) => {
                let global_str =
                    llvm_err(self.engine.builder.build_global_string_ptr(text, "str"))?;
                Ok(global_str.as_pointer_value().into())
            }
            Expression::Identifier(name, _) => {
                let (ptr, ty) = self.local_vars.get(name).ok_or_else(|| {
                    MatcError::CodegenError(format!("Undefined variable in codegen: {}", name))
                })?;
                let llvm_ty = self.engine.llvm_type(ty);
                let loaded = llvm_err(self.engine.builder.build_load(llvm_ty, *ptr, name))?;
                Ok(loaded)
            }
            Expression::Binary {
                op, left, right, ..
            } => {
                let left_val = self.compile_expression(left)?;
                let right_val = self.compile_expression(right)?;
                self.compile_binary_expr(op, left_val, right_val)
            }
            Expression::TupleLiteral(elements, _) => self.compile_tuple_literal(elements),
            Expression::ArrayLiteral(elements, _) => self.compile_array_literal(elements),
            Expression::TupleAccess { expr, index, .. } => self.compile_tuple_access(expr, *index),
            Expression::ArrayAccess { expr, index, .. } => self.compile_array_access(expr, index),
            Expression::InterpolatedString(parts, _) => self.compile_interpolated_string(parts),
            Expression::Ok(val_expr, _) => self.compile_ok_expr(val_expr),
            Expression::Err(err_expr, _) => self.compile_err_expr(err_expr),
            Expression::Call {
                callee, arguments, ..
            } => self.compile_call_expr(callee, arguments),
        }
    }
}
