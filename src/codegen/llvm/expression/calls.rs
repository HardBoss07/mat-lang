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

        if callee == "Vec::new" {
            let elem_size = self.engine.context.i64_type().const_int(8, false);
            let init_cap = self.engine.context.i64_type().const_int(4, false);
            let create_fn = self
                .engine
                .module
                .get_function("_mat_rt_vec_create")
                .unwrap();
            let call_site = llvm_err(self.engine.builder.build_call(
                create_fn,
                &[elem_size.into(), init_cap.into()],
                "call_vec_new",
            ))?;
            return Ok(unsafe { BasicValueEnum::new(call_site.as_value_ref()) });
        }

        if let Some(last_colon) = callee.rfind("::") {
            let enum_name = &callee[..last_colon];
            let variant_name = &callee[last_colon + 2..];
            if let Some(enum_sym) = self.symbol_table.lookup_enum(enum_name).cloned() {
                if let Some((tag_index, variant_def)) = enum_sym
                    .variants
                    .iter()
                    .enumerate()
                    .find(|(_, v)| v.name == variant_name)
                {
                    let short_e_name = enum_name.split("::").last().unwrap_or(enum_name);
                    let map = self.engine.enum_types.borrow();
                    let mut enum_struct_ty = map
                        .get(enum_name)
                        .or_else(|| map.get(short_e_name))
                        .copied();
                    if enum_struct_ty.is_none() {
                        let suffix = format!("::{}", short_e_name);
                        for (k, &ty) in map.iter() {
                            if k.ends_with(&suffix) {
                                enum_struct_ty = Some(ty);
                                break;
                            }
                        }
                    }
                    let enum_struct_ty = enum_struct_ty.ok_or_else(|| {
                        MatcError::CodegenError(format!(
                            "LLVM struct type for enum '{}' not found",
                            enum_name
                        ))
                    })?;

                    let enum_alloca =
                        self.create_entry_block_alloca(enum_struct_ty.into(), "enum_tmp")?;

                    let tag_val = self
                        .engine
                        .context
                        .i32_type()
                        .const_int(tag_index as u64, false);
                    let tag_ptr = llvm_err(self.engine.builder.build_struct_gep(
                        enum_struct_ty,
                        enum_alloca,
                        0,
                        "enum_tag_ptr",
                    ))?;
                    llvm_err(self.engine.builder.build_store(tag_ptr, tag_val))?;

                    let payload_buf_ptr = llvm_err(self.engine.builder.build_struct_gep(
                        enum_struct_ty,
                        enum_alloca,
                        1,
                        "enum_payload_ptr",
                    ))?;

                    let payload_field_tys: Vec<_> = match &variant_def.payload {
                        crate::ast::EnumVariantPayload::Tuple(tys) => {
                            tys.iter().map(|t| self.engine.llvm_type(t)).collect()
                        }
                        _ => vec![],
                    };

                    if !payload_field_tys.is_empty() {
                        let payload_struct_ty =
                            self.engine.context.struct_type(&payload_field_tys, false);
                        let ptr_ty = self
                            .engine
                            .context
                            .ptr_type(inkwell::AddressSpace::default());
                        let cast_ptr = llvm_err(self.engine.builder.build_bit_cast(
                            payload_buf_ptr,
                            ptr_ty,
                            "payload_cast",
                        ))?
                        .into_pointer_value();

                        for (idx, arg_expr) in arguments.iter().enumerate() {
                            let field_mat_ty = match &variant_def.payload {
                                crate::ast::EnumVariantPayload::Tuple(tys) => {
                                    tys.get(idx).cloned().unwrap_or(Type::Int)
                                }
                                _ => Type::Int,
                            };
                            let raw_val = self.compile_expression(arg_expr)?;
                            let val = super::super::util::coerce_val_to_type(
                                self.engine,
                                raw_val,
                                &field_mat_ty,
                            )?;
                            let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
                                payload_struct_ty,
                                cast_ptr,
                                idx as u32,
                                "enum_field_ptr",
                            ))?;
                            llvm_err(self.engine.builder.build_store(field_ptr, val))?;
                        }
                    }

                    let loaded = llvm_err(self.engine.builder.build_load(
                        enum_struct_ty,
                        enum_alloca,
                        "enum_val",
                    ))?;
                    return Ok(loaded);
                }
            }
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
