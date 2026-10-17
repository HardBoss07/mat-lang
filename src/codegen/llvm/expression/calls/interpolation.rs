//! Interpolated string expression code generation.
//!
//! This module compiles string interpolation AST expressions into runtime format string calls.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::{Expression, FormatSpecifier};
use crate::error::Result;
use inkwell::values::{AsValueRef, BasicMetadataValueEnum, BasicValueEnum};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles an interpolated string expression into an LLVM runtime format string call.
    ///
    /// # Arguments
    ///
    /// * `parts` - The interpolated string parts and their format specifiers.
    ///
    /// # Returns
    ///
    /// A pointer value representing the formatted string.
    ///
    /// # Errors
    ///
    /// Returns a [`crate::error::MatcError`] if LLVM instruction building fails.
    pub(crate) fn compile_interpolated_string(
        &mut self,
        parts: &[(Expression, FormatSpecifier)],
    ) -> Result<BasicValueEnum<'ctx>> {
        let mut fmt_string = String::new();
        let mut args: Vec<BasicValueEnum<'ctx>> = Vec::new();

        for (part, specifier) in parts {
            match part {
                Expression::StringLiteral(literal_str, _) => {
                    fmt_string.push_str(&literal_str.replace('%', "%%"));
                }
                other_expr => {
                    self.format_interpolated_argument(
                        other_expr,
                        specifier,
                        &mut fmt_string,
                        &mut args,
                    )?;
                }
            }
        }

        self.call_runtime_fmt_string(&fmt_string, &args)
    }

    /// Formats a single non-literal argument within an interpolated string expression.
    fn format_interpolated_argument(
        &mut self,
        expression: &Expression,
        specifier: &FormatSpecifier,
        fmt_string: &mut String,
        args: &mut Vec<BasicValueEnum<'ctx>>,
    ) -> Result<()> {
        let val = self.compile_expression(expression)?;
        match specifier {
            FormatSpecifier::Bin => self.format_bin_specifier(val, fmt_string, args),
            FormatSpecifier::Hex => self.format_hex_specifier(val, fmt_string, args),
            FormatSpecifier::None => self.format_default_specifier(val, fmt_string, args),
        }
    }

    /// Formats an integer argument using binary formatting.
    fn format_bin_specifier(
        &mut self,
        val: BasicValueEnum<'ctx>,
        fmt_string: &mut String,
        args: &mut Vec<BasicValueEnum<'ctx>>,
    ) -> Result<()> {
        let i64_val = self.extend_int_to_i64(val.into_int_value())?;
        let fmt_bin_fn = self.engine.module.get_function("_mat_rt_fmt_bin").unwrap();
        let call_res = llvm_err(self.engine.builder.build_call(
            fmt_bin_fn,
            &[i64_val.into()],
            "call_fmt_bin",
        ))?;
        let formatted_ptr = unsafe { BasicValueEnum::new(call_res.as_value_ref()) };
        fmt_string.push_str("%s");
        args.push(formatted_ptr);
        Ok(())
    }

    /// Formats an integer argument using hexadecimal formatting.
    fn format_hex_specifier(
        &mut self,
        val: BasicValueEnum<'ctx>,
        fmt_string: &mut String,
        args: &mut Vec<BasicValueEnum<'ctx>>,
    ) -> Result<()> {
        let i64_val = self.extend_int_to_i64(val.into_int_value())?;
        let fmt_hex_fn = self.engine.module.get_function("_mat_rt_fmt_hex").unwrap();
        let call_res = llvm_err(self.engine.builder.build_call(
            fmt_hex_fn,
            &[i64_val.into()],
            "call_fmt_hex",
        ))?;
        let formatted_ptr = unsafe { BasicValueEnum::new(call_res.as_value_ref()) };
        fmt_string.push_str("%s");
        args.push(formatted_ptr);
        Ok(())
    }

    /// Formats an argument with default type-directed format specifiers.
    fn format_default_specifier(
        &mut self,
        val: BasicValueEnum<'ctx>,
        fmt_string: &mut String,
        args: &mut Vec<BasicValueEnum<'ctx>>,
    ) -> Result<()> {
        if val.is_int_value() {
            let int_val = val.into_int_value();
            if int_val.get_type().get_bit_width() == 1 {
                fmt_string.push_str("%s");
                let bool_str = self.format_bool_to_string_ptr(int_val)?;
                args.push(bool_str);
            } else {
                fmt_string.push_str("%lld");
                let i64_val = if int_val.get_type().get_bit_width() < 64 {
                    self.extend_int_to_i64(int_val)?.into()
                } else {
                    val
                };
                args.push(i64_val);
            }
        } else if val.is_float_value() {
            fmt_string.push_str("%g");
            args.push(val);
        } else if val.is_pointer_value() {
            fmt_string.push_str("%s");
            args.push(val);
        }
        Ok(())
    }

    /// Extends integer values narrower than 64-bit to i64.
    fn extend_int_to_i64(
        &mut self,
        int_val: inkwell::values::IntValue<'ctx>,
    ) -> Result<inkwell::values::IntValue<'ctx>> {
        if int_val.get_type().get_bit_width() < 64 {
            llvm_err(self.engine.builder.build_int_s_extend(
                int_val,
                self.engine.context.i64_type(),
                "i64_ext",
            ))
        } else {
            Ok(int_val)
        }
    }

    /// Selects string pointer "tru" or "fal" based on boolean value.
    fn format_bool_to_string_ptr(
        &mut self,
        int_val: inkwell::values::IntValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let tru_ptr = llvm_err(
            self.engine
                .builder
                .build_global_string_ptr("tru", "str_tru"),
        )?
        .as_pointer_value();
        let fal_ptr = llvm_err(
            self.engine
                .builder
                .build_global_string_ptr("fal", "str_fal"),
        )?
        .as_pointer_value();

        llvm_err(
            self.engine
                .builder
                .build_select(int_val, tru_ptr, fal_ptr, "bool_str"),
        )
    }

    /// Invokes the runtime format string function with format string and metadata args.
    fn call_runtime_fmt_string(
        &mut self,
        fmt_string: &str,
        args: &[BasicValueEnum<'ctx>],
    ) -> Result<BasicValueEnum<'ctx>> {
        let fmt_fn = self
            .engine
            .module
            .get_function("_mat_rt_fmt_string")
            .unwrap();
        let fmt_ptr = llvm_err(
            self.engine
                .builder
                .build_global_string_ptr(fmt_string, "fmt"),
        )?;

        let mut call_args: Vec<BasicMetadataValueEnum<'ctx>> =
            vec![fmt_ptr.as_pointer_value().into()];
        for arg in args {
            call_args.push((*arg).into());
        }

        let call_fmt = llvm_err(
            self.engine
                .builder
                .build_call(fmt_fn, &call_args, "call_fmt"),
        )?;

        let res_ptr = unsafe { BasicValueEnum::new(call_fmt.as_value_ref()) };
        Ok(res_ptr)
    }
}
