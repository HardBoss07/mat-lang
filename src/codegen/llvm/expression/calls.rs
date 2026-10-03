use super::super::function::FunctionCompiler;
use super::super::util::llvm_err;
use crate::ast::{Expression, FormatSpecifier, Type};
use crate::codegen::mangling::mangle_symbol;
use crate::error::{MatcError, Result};
use inkwell::values::{AsValueRef, BasicMetadataValueEnum, BasicValueEnum};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_interpolated_string(
        &mut self,
        parts: &[(Expression, FormatSpecifier)],
    ) -> Result<BasicValueEnum<'ctx>> {
        let mut fmt_string = String::new();
        let mut args: Vec<BasicValueEnum<'ctx>> = Vec::new();

        for (part, specifier) in parts {
            match part {
                Expression::StringLiteral(s, _) => {
                    fmt_string.push_str(&s.replace('%', "%%"));
                }
                other => {
                    let val = self.compile_expression(other)?;
                    match specifier {
                        FormatSpecifier::Bin => {
                            let int_val = val.into_int_value();
                            let i64_val = if int_val.get_type().get_bit_width() < 64 {
                                llvm_err(self.engine.builder.build_int_s_extend(
                                    int_val,
                                    self.engine.context.i64_type(),
                                    "i64_ext",
                                ))?
                            } else {
                                int_val
                            };
                            let fmt_bin_fn =
                                self.engine.module.get_function("_mat_rt_fmt_bin").unwrap();
                            let call_res = llvm_err(self.engine.builder.build_call(
                                fmt_bin_fn,
                                &[i64_val.into()],
                                "call_fmt_bin",
                            ))?;
                            let formatted_ptr =
                                unsafe { BasicValueEnum::new(call_res.as_value_ref()) };
                            fmt_string.push_str("%s");
                            args.push(formatted_ptr);
                        }
                        FormatSpecifier::Hex => {
                            let int_val = val.into_int_value();
                            let i64_val = if int_val.get_type().get_bit_width() < 64 {
                                llvm_err(self.engine.builder.build_int_s_extend(
                                    int_val,
                                    self.engine.context.i64_type(),
                                    "i64_ext",
                                ))?
                            } else {
                                int_val
                            };
                            let fmt_hex_fn =
                                self.engine.module.get_function("_mat_rt_fmt_hex").unwrap();
                            let call_res = llvm_err(self.engine.builder.build_call(
                                fmt_hex_fn,
                                &[i64_val.into()],
                                "call_fmt_hex",
                            ))?;
                            let formatted_ptr =
                                unsafe { BasicValueEnum::new(call_res.as_value_ref()) };
                            fmt_string.push_str("%s");
                            args.push(formatted_ptr);
                        }
                        FormatSpecifier::None => {
                            if val.is_int_value() {
                                let int_val = val.into_int_value();
                                if int_val.get_type().get_bit_width() == 1 {
                                    fmt_string.push_str("%s");
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

                                    let bool_str = llvm_err(
                                        self.engine
                                            .builder
                                            .build_select(int_val, tru_ptr, fal_ptr, "bool_str"),
                                    )?;
                                    args.push(bool_str);
                                } else {
                                    fmt_string.push_str("%lld");
                                    let i64_val = if int_val.get_type().get_bit_width() < 64 {
                                        llvm_err(self.engine.builder.build_int_s_extend(
                                            int_val,
                                            self.engine.context.i64_type(),
                                            "i64_ext",
                                        ))?
                                        .into()
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
                        }
                    }
                }
            }
        }

        let fmt_fn = self
            .engine
            .module
            .get_function("_mat_rt_fmt_string")
            .unwrap();
        let fmt_ptr = llvm_err(
            self.engine
                .builder
                .build_global_string_ptr(&fmt_string, "fmt"),
        )?;

        let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
            vec![fmt_ptr.as_pointer_value().into()];
        for arg in &args {
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

    pub(crate) fn compile_ok_expr(
        &mut self,
        val_expr: &Expression,
    ) -> Result<BasicValueEnum<'ctx>> {
        let inner_val = self.compile_expression(val_expr)?;
        let ok_ty = self
            .type_checker
            .synthesize_expr(val_expr, &self.symbol_table)
            .unwrap_or(Type::Int);
        let result_mat_ty = Type::Result(Box::new(ok_ty), Box::new(Type::String));
        let result_llvm_ty = self.engine.llvm_type(&result_mat_ty);

        let alloca = self.create_entry_block_alloca(result_llvm_ty, "ok_tmp")?;

        let tag_ptr = llvm_err(self.engine.builder.build_struct_gep(
            result_llvm_ty.into_struct_type(),
            alloca,
            0,
            "tag_ptr",
        ))?;
        let tru_val = self.engine.context.bool_type().const_int(1, false);
        llvm_err(self.engine.builder.build_store(tag_ptr, tru_val))?;

        let val_ptr = llvm_err(self.engine.builder.build_struct_gep(
            result_llvm_ty.into_struct_type(),
            alloca,
            1,
            "val_ptr",
        ))?;
        llvm_err(self.engine.builder.build_store(val_ptr, inner_val))?;

        let loaded = llvm_err(
            self.engine
                .builder
                .build_load(result_llvm_ty, alloca, "ok_struct"),
        )?;
        Ok(loaded)
    }

    pub(crate) fn compile_err_expr(
        &mut self,
        err_expr: &Expression,
    ) -> Result<BasicValueEnum<'ctx>> {
        let inner_err = self.compile_expression(err_expr)?;
        let err_ty = self
            .type_checker
            .synthesize_expr(err_expr, &self.symbol_table)
            .unwrap_or(Type::String);
        let result_mat_ty = Type::Result(Box::new(Type::Int), Box::new(err_ty));
        let result_llvm_ty = self.engine.llvm_type(&result_mat_ty);

        let alloca = self.create_entry_block_alloca(result_llvm_ty, "err_tmp")?;

        let tag_ptr = llvm_err(self.engine.builder.build_struct_gep(
            result_llvm_ty.into_struct_type(),
            alloca,
            0,
            "tag_ptr",
        ))?;
        let fal_val = self.engine.context.bool_type().const_int(0, false);
        llvm_err(self.engine.builder.build_store(tag_ptr, fal_val))?;

        let err_ptr = llvm_err(self.engine.builder.build_struct_gep(
            result_llvm_ty.into_struct_type(),
            alloca,
            2,
            "err_ptr",
        ))?;
        llvm_err(self.engine.builder.build_store(err_ptr, inner_err))?;

        let loaded = llvm_err(self.engine.builder.build_load(
            result_llvm_ty,
            alloca,
            "err_struct",
        ))?;
        Ok(loaded)
    }

    pub(crate) fn compile_call_expr(
        &mut self,
        callee: &str,
        arguments: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        if callee == "println" {
            if let Some(first_arg) = arguments.first() {
                let val = self.compile_expression(first_arg)?;
                let println_str_fn = self
                    .engine
                    .module
                    .get_function("_mat_rt_println_str")
                    .unwrap();
                let println_int_fn = self
                    .engine
                    .module
                    .get_function("_mat_rt_println_int")
                    .unwrap();
                let println_float_fn = self
                    .engine
                    .module
                    .get_function("_mat_rt_println_float")
                    .unwrap();
                let println_bool_fn = self
                    .engine
                    .module
                    .get_function("_mat_rt_println_bool")
                    .unwrap();

                if val.is_pointer_value() {
                    llvm_err(self.engine.builder.build_call(
                        println_str_fn,
                        &[val.into()],
                        "call_println_str",
                    ))?;
                } else if val.is_int_value() {
                    let int_val = val.into_int_value();
                    if int_val.get_type().get_bit_width() == 1 {
                        llvm_err(self.engine.builder.build_call(
                            println_bool_fn,
                            &[int_val.into()],
                            "call_println_bool",
                        ))?;
                    } else {
                        let i64_val = if int_val.get_type().get_bit_width() < 64 {
                            llvm_err(self.engine.builder.build_int_s_extend(
                                int_val,
                                self.engine.context.i64_type(),
                                "i64_ext",
                            ))?
                            .into()
                        } else {
                            val
                        };
                        llvm_err(self.engine.builder.build_call(
                            println_int_fn,
                            &[i64_val.into()],
                            "call_println_int",
                        ))?;
                    }
                } else if val.is_float_value() {
                    llvm_err(self.engine.builder.build_call(
                        println_float_fn,
                        &[val.into()],
                        "call_println_float",
                    ))?;
                }
            }
            return Ok(self.engine.context.i32_type().const_int(0, false).into());
        }

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
