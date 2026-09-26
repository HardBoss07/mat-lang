use super::function::FunctionCompiler;
use crate::ast::{Expression, Type};
use crate::error::{MatcError, Result};
use inkwell::types::BasicType;
use inkwell::values::{AsValueRef, BasicValueEnum};

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
                let global_str = self
                    .engine
                    .builder
                    .build_global_string_ptr(text, "str")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(global_str.as_pointer_value().into())
            }
            Expression::Identifier(name, _) => {
                let (ptr, ty) = self.local_vars.get(name).ok_or_else(|| {
                    MatcError::CodegenError(format!("Undefined variable in codegen: {}", name))
                })?;
                let llvm_ty = self.engine.llvm_type(ty);
                let loaded = self
                    .engine
                    .builder
                    .build_load(llvm_ty, *ptr, name)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::TupleLiteral(elements, _) => {
                let mut field_values = Vec::new();
                let mut field_types = Vec::new();
                for elem in elements {
                    let val = self.compile_expression(elem)?;
                    field_types.push(val.get_type());
                    field_values.push(val);
                }

                let struct_ty = self.engine.context.struct_type(&field_types, false);
                let alloca = self
                    .engine
                    .builder
                    .build_alloca(struct_ty, "tuple_tmp")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                for (idx, val) in field_values.into_iter().enumerate() {
                    let field_ptr = self
                        .engine
                        .builder
                        .build_struct_gep(struct_ty, alloca, idx as u32, "tuple_gep")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                    self.engine
                        .builder
                        .build_store(field_ptr, val)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                let loaded = self
                    .engine
                    .builder
                    .build_load(struct_ty, alloca, "tuple_val")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::ArrayLiteral(elements, _) => {
                if elements.is_empty() {
                    return Err(MatcError::CodegenError(
                        "Cannot codegen empty array".to_string(),
                    ));
                }

                let mut compiled_elements = Vec::new();
                for elem in elements {
                    compiled_elements.push(self.compile_expression(elem)?);
                }

                let elem_llvm_ty = compiled_elements[0].get_type();
                let array_ty = elem_llvm_ty.array_type(elements.len() as u32);
                let alloca = self
                    .engine
                    .builder
                    .build_alloca(array_ty, "arr_tmp")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let zero = self.engine.context.i32_type().const_int(0, false);
                for (idx, val) in compiled_elements.into_iter().enumerate() {
                    let idx_val = self.engine.context.i32_type().const_int(idx as u64, false);
                    let elem_ptr = unsafe {
                        self.engine
                            .builder
                            .build_gep(array_ty, alloca, &[zero, idx_val], "arr_gep")
                            .map_err(|e| MatcError::CodegenError(e.to_string()))?
                    };
                    self.engine
                        .builder
                        .build_store(elem_ptr, val)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                let loaded = self
                    .engine
                    .builder
                    .build_load(array_ty, alloca, "arr_val")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::TupleAccess { expr, index, .. } => {
                let tuple_val = self.compile_expression(expr)?;
                if let BasicValueEnum::StructValue(struct_val) = tuple_val {
                    let extracted = self
                        .engine
                        .builder
                        .build_extract_value(struct_val, *index as u32, "tuple_extract")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                    Ok(extracted)
                } else {
                    Err(MatcError::CodegenError(
                        "Expected struct value for tuple access".to_string(),
                    ))
                }
            }
            Expression::ArrayAccess { expr, index, .. } => {
                let array_mat_ty = self
                    .type_checker
                    .synthesize_expr(expr, &self.symbol_table)?;
                let elem_mat_ty = match array_mat_ty {
                    Type::Array(ref elem, _) => *elem.clone(),
                    _ => return Err(MatcError::CodegenError("Expected array type".to_string())),
                };

                let array_llvm_ty = self.engine.llvm_type(&array_mat_ty);
                let elem_llvm_ty = self.engine.llvm_type(&elem_mat_ty);

                let array_val = self.compile_expression(expr)?;
                let index_val = self.compile_expression(index)?.into_int_value();

                let alloca = self
                    .engine
                    .builder
                    .build_alloca(array_llvm_ty, "arr_access_tmp")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                self.engine
                    .builder
                    .build_store(alloca, array_val)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let zero = self.engine.context.i32_type().const_int(0, false);
                let elem_ptr = unsafe {
                    self.engine
                        .builder
                        .build_gep(array_llvm_ty, alloca, &[zero, index_val], "arr_elem_gep")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                };

                let loaded = self
                    .engine
                    .builder
                    .build_load(elem_llvm_ty, elem_ptr, "arr_elem_val")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::InterpolatedString(parts, _) => {
                let mut fmt_string = String::new();
                let mut args: Vec<BasicValueEnum<'ctx>> = Vec::new();

                for part in parts {
                    match part {
                        Expression::StringLiteral(s, _) => {
                            fmt_string.push_str(&s.replace('%', "%%"))
                        }
                        other => {
                            let val = self.compile_expression(other)?;
                            if val.is_int_value() {
                                let int_val = val.into_int_value();
                                if int_val.get_type().get_bit_width() == 1 {
                                    fmt_string.push_str("%s");
                                    let tru_ptr = self
                                        .engine
                                        .builder
                                        .build_global_string_ptr("tru", "str_tru")
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                                        .as_pointer_value();
                                    let fal_ptr = self
                                        .engine
                                        .builder
                                        .build_global_string_ptr("fal", "str_fal")
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                                        .as_pointer_value();

                                    let bool_str = self
                                        .engine
                                        .builder
                                        .build_select(int_val, tru_ptr, fal_ptr, "bool_str")
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                                    args.push(bool_str);
                                } else {
                                    fmt_string.push_str("%lld");
                                    let i64_val = if int_val.get_type().get_bit_width() < 64 {
                                        self.engine
                                            .builder
                                            .build_int_s_extend(
                                                int_val,
                                                self.engine.context.i64_type(),
                                                "i64_ext",
                                            )
                                            .map_err(|e| MatcError::CodegenError(e.to_string()))?
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

                let fmt_fn = self
                    .engine
                    .module
                    .get_function("_mat_rt_fmt_string")
                    .unwrap();
                let fmt_ptr = self
                    .engine
                    .builder
                    .build_global_string_ptr(&fmt_string, "fmt")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                    vec![fmt_ptr.as_pointer_value().into()];
                for arg in &args {
                    call_args.push((*arg).into());
                }

                let call_fmt = self
                    .engine
                    .builder
                    .build_call(fmt_fn, &call_args, "call_fmt")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let res_ptr = unsafe { BasicValueEnum::new(call_fmt.as_value_ref()) };
                Ok(res_ptr)
            }
            Expression::Call {
                callee, arguments, ..
            } => {
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
                            self.engine
                                .builder
                                .build_call(println_str_fn, &[val.into()], "call_println_str")
                                .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                        } else if val.is_int_value() {
                            let int_val = val.into_int_value();
                            if int_val.get_type().get_bit_width() == 1 {
                                self.engine
                                    .builder
                                    .build_call(
                                        println_bool_fn,
                                        &[int_val.into()],
                                        "call_println_bool",
                                    )
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                            } else {
                                let i64_val = if int_val.get_type().get_bit_width() < 64 {
                                    self.engine
                                        .builder
                                        .build_int_s_extend(
                                            int_val,
                                            self.engine.context.i64_type(),
                                            "i64_ext",
                                        )
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                                        .into()
                                } else {
                                    val
                                };
                                self.engine
                                    .builder
                                    .build_call(
                                        println_int_fn,
                                        &[i64_val.into()],
                                        "call_println_int",
                                    )
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                            }
                        } else if val.is_float_value() {
                            self.engine
                                .builder
                                .build_call(println_float_fn, &[val.into()], "call_println_float")
                                .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                        }
                    }
                }
                Ok(self.engine.context.i32_type().const_int(0, false).into())
            }
        }
    }
}
