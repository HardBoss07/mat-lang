//! Function and constructor call code generation.
//!
//! This module coordinates compiling user function calls, built-ins,
//! enum constructors, and result expressions into LLVM IR.

pub mod builtins;
pub mod enum_calls;
pub mod interpolation;
pub mod result_ops;

#[allow(unused_imports)]
pub(crate) use builtins::*;
#[allow(unused_imports)]
pub(crate) use enum_calls::*;
#[allow(unused_imports)]
pub(crate) use interpolation::*;
#[allow(unused_imports)]
pub(crate) use result_ops::*;

use super::super::function::FunctionCompiler;
use super::super::util::llvm_err;
use crate::ast::{Expression, Span};
use crate::codegen::mangling::mangle_symbol;
use crate::error::{MatcError, Result};
use inkwell::values::{AsValueRef, BasicMetadataValueEnum, BasicValueEnum};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles a general call expression into an LLVM call instruction.
    ///
    /// # Arguments
    ///
    /// * `callee` - The name or qualified path of the callee.
    /// * `arguments` - The argument expressions.
    ///
    /// # Returns
    ///
    /// The return value of the call or a dummy zero for void routines.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if the function is not found or LLVM call construction fails.
    pub(crate) fn compile_call_expr(
        &mut self,
        callee: &str,
        arguments: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        if callee == "println" {
            return self.compile_builtin_println(arguments);
        }

        if callee == "Vec::new" {
            let synth_call = Expression::Call {
                callee: callee.to_string(),
                arguments: arguments.to_vec(),
                span: Span::new(0, 0),
            };
            return self.compile_builtin_vec_new(&synth_call, None);
        }

        if let Some(enum_res) = self.try_compile_enum_constructor(callee, arguments) {
            return enum_res;
        }

        self.compile_user_function_call(callee, arguments)
    }

    /// Compiles a call to a user-defined function.
    fn compile_user_function_call(
        &mut self,
        callee: &str,
        arguments: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        let mangled = mangle_symbol(callee);
        let target_fn =
            self.engine.module.get_function(&mangled).ok_or_else(|| {
                MatcError::CodegenError(format!("Function not found: {}", callee))
            })?;

        let mut compiled_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
        for arg in arguments {
            compiled_args.push(self.compile_expression(arg)?.into());
        }

        let call_site = llvm_err(self.engine.builder.build_call(
            target_fn,
            &compiled_args,
            "calltmp",
        ))?;

        if target_fn.get_type().get_return_type().is_some() {
            Ok(unsafe { BasicValueEnum::new(call_site.as_value_ref()) })
        } else {
            Ok(self.engine.context.i32_type().const_int(0, false).into())
        }
    }
}
