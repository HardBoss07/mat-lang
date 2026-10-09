use super::super::function::FunctionCompiler;
use super::super::util::llvm_err;
use crate::ast::Expression;
use crate::error::{MatcError, Result};
use inkwell::types::BasicType;
use inkwell::values::{AsValueRef, BasicValueEnum};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_tuple_literal(
        &mut self,
        elements: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        let mut field_values = Vec::new();
        let mut field_types = Vec::new();
        for elem in elements {
            let val = self.compile_expression(elem)?;
            field_types.push(val.get_type());
            field_values.push(val);
        }

        let struct_ty = self.engine.context.struct_type(&field_types, false);
        let alloca = self.create_entry_block_alloca(struct_ty.into(), "tuple_tmp")?;

        for (idx, val) in field_values.into_iter().enumerate() {
            let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
                struct_ty,
                alloca,
                idx as u32,
                "tuple_gep",
            ))?;
            llvm_err(self.engine.builder.build_store(field_ptr, val))?;
        }

        let loaded = llvm_err(
            self.engine
                .builder
                .build_load(struct_ty, alloca, "tuple_val"),
        )?;
        Ok(loaded)
    }

    pub(crate) fn compile_array_literal(
        &mut self,
        elements: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
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
        let alloca = self.create_entry_block_alloca(array_ty.into(), "arr_tmp")?;

        let zero = self.engine.context.i32_type().const_int(0, false);
        for (idx, val) in compiled_elements.into_iter().enumerate() {
            let idx_val = self.engine.context.i32_type().const_int(idx as u64, false);
            let elem_ptr = unsafe {
                llvm_err(self.engine.builder.build_gep(
                    array_ty,
                    alloca,
                    &[zero, idx_val],
                    "arr_gep",
                ))?
            };
            llvm_err(self.engine.builder.build_store(elem_ptr, val))?;
        }

        let loaded = llvm_err(self.engine.builder.build_load(array_ty, alloca, "arr_val"))?;
        Ok(loaded)
    }

    pub(crate) fn compile_vector_literal(
        &mut self,
        elements: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        let vec_mat_ty = self.type_checker.synthesize_expr(
            &Expression::VectorLiteral(elements.to_vec(), crate::ast::Span::new(0, 0)),
            &self.symbol_table,
        )?;

        let elem_mat_ty = match &vec_mat_ty {
            crate::ast::Type::Vector(e) => (**e).clone(),
            _ => crate::ast::Type::Int,
        };

        let elem_llvm_ty = self.engine.llvm_type(&elem_mat_ty);
        let elem_size = elem_llvm_ty.size_of().ok_or_else(|| {
            MatcError::CodegenError("Cannot determine size of vector element type".to_string())
        })?;

        let create_fn = self
            .engine
            .module
            .get_function("_mat_rt_vec_create")
            .unwrap();
        let push_fn = self.engine.module.get_function("_mat_rt_vec_push").unwrap();

        let initial_cap = self
            .engine
            .context
            .i64_type()
            .const_int(elements.len().max(4) as u64, false);

        let call_create = llvm_err(self.engine.builder.build_call(
            create_fn,
            &[elem_size.into(), initial_cap.into()],
            "call_vec_create",
        ))?;
        let vec_ptr = unsafe { BasicValueEnum::new(call_create.as_value_ref()) };

        for elem in elements {
            let val = self.compile_expression(elem)?;
            let elem_alloca = self.create_entry_block_alloca(elem_llvm_ty, "elem_tmp")?;
            llvm_err(self.engine.builder.build_store(elem_alloca, val))?;
            llvm_err(self.engine.builder.build_call(
                push_fn,
                &[vec_ptr.into(), elem_alloca.into()],
                "call_vec_push",
            ))?;
        }

        Ok(vec_ptr)
    }

    pub(crate) fn compile_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Expression)],
    ) -> Result<BasicValueEnum<'ctx>> {
        let struct_def = self
            .symbol_table
            .lookup_struct(name)
            .ok_or_else(|| MatcError::CodegenError(format!("Struct '{}' not found", name)))?
            .clone();

        let short_name = name.split("::").last().unwrap_or(name);
        let struct_map = self.engine.struct_types.borrow();
        let mut struct_ty = struct_map
            .get(name)
            .or_else(|| struct_map.get(short_name))
            .copied();
        if struct_ty.is_none() {
            let suffix = format!("::{}", short_name);
            for (k, &ty) in struct_map.iter() {
                if k.ends_with(&suffix) {
                    struct_ty = Some(ty);
                    break;
                }
            }
        }

        let struct_ty = struct_ty.ok_or_else(|| {
            MatcError::CodegenError(format!("LLVM struct type '{}' not found", name))
        })?;

        let alloca = self.create_entry_block_alloca(struct_ty.into(), "struct_tmp")?;

        for (field_name, field_expr) in fields {
            let (field_idx, (_, field_ty)) = struct_def
                .fields
                .iter()
                .enumerate()
                .find(|(_, (n, _))| n == field_name)
                .ok_or_else(|| {
                    MatcError::CodegenError(format!(
                        "Field '{}' not found in struct '{}'",
                        field_name, name
                    ))
                })?;

            let raw_val = self.compile_expression(field_expr)?;
            let val = super::super::util::coerce_val_to_type(self.engine, raw_val, field_ty)?;

            let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
                struct_ty,
                alloca,
                field_idx as u32,
                "field_init_gep",
            ))?;
            llvm_err(self.engine.builder.build_store(field_ptr, val))?;
        }

        let loaded = llvm_err(
            self.engine
                .builder
                .build_load(struct_ty, alloca, "struct_val"),
        )?;
        Ok(loaded)
    }

    pub(crate) fn compile_method_call(
        &mut self,
        receiver: &Expression,
        method: &str,
        args: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        let receiver_mat_ty = self
            .type_checker
            .synthesize_expr(receiver, &self.symbol_table)?;

        match (&receiver_mat_ty, method) {
            (crate::ast::Type::Vector(elem_mat_ty), "push") => {
                let vec_val = self.compile_expression(receiver)?;
                let elem_llvm_ty = self.engine.llvm_type(elem_mat_ty);
                let push_fn = self.engine.module.get_function("_mat_rt_vec_push").unwrap();

                for arg in args {
                    let val = self.compile_expression(arg)?;
                    let coerced =
                        super::super::util::coerce_val_to_type(self.engine, val, elem_mat_ty)?;
                    let arg_alloca = self.create_entry_block_alloca(elem_llvm_ty, "push_arg")?;
                    llvm_err(self.engine.builder.build_store(arg_alloca, coerced))?;
                    llvm_err(self.engine.builder.build_call(
                        push_fn,
                        &[vec_val.into(), arg_alloca.into()],
                        "call_push",
                    ))?;
                }
                Ok(self.engine.context.i32_type().const_int(0, false).into())
            }
            (crate::ast::Type::Vector(elem_mat_ty), "pop") => {
                let vec_val = self.compile_expression(receiver)?;
                let elem_llvm_ty = self.engine.llvm_type(elem_mat_ty);
                let out_alloca = self.create_entry_block_alloca(elem_llvm_ty, "pop_out")?;

                let pop_fn = self.engine.module.get_function("_mat_rt_vec_pop").unwrap();
                llvm_err(self.engine.builder.build_call(
                    pop_fn,
                    &[vec_val.into(), out_alloca.into()],
                    "call_pop",
                ))?;

                let loaded = llvm_err(self.engine.builder.build_load(
                    elem_llvm_ty,
                    out_alloca,
                    "pop_val",
                ))?;
                Ok(loaded)
            }
            (crate::ast::Type::Vector(elem_mat_ty), "get") => {
                let vec_val = self.compile_expression(receiver)?;
                let elem_llvm_ty = self.engine.llvm_type(elem_mat_ty);
                let index_val = self.compile_expression(&args[0])?.into_int_value();
                let i64_index = if index_val.get_type().get_bit_width() < 64 {
                    llvm_err(self.engine.builder.build_int_s_extend(
                        index_val,
                        self.engine.context.i64_type(),
                        "idx_ext",
                    ))?
                } else {
                    index_val
                };

                let get_fn = self.engine.module.get_function("_mat_rt_vec_get").unwrap();
                let call_res = llvm_err(self.engine.builder.build_call(
                    get_fn,
                    &[vec_val.into(), i64_index.into()],
                    "call_vec_get",
                ))?;
                let elem_ptr =
                    unsafe { BasicValueEnum::new(call_res.as_value_ref()) }.into_pointer_value();
                let loaded = llvm_err(self.engine.builder.build_load(
                    elem_llvm_ty,
                    elem_ptr,
                    "vec_elem_val",
                ))?;
                Ok(loaded)
            }
            (crate::ast::Type::Vector(_), "len") => {
                let vec_val = self.compile_expression(receiver)?;
                let len_fn = self.engine.module.get_function("_mat_rt_vec_len").unwrap();
                let call_res = llvm_err(self.engine.builder.build_call(
                    len_fn,
                    &[vec_val.into()],
                    "call_vec_len",
                ))?;
                Ok(unsafe { BasicValueEnum::new(call_res.as_value_ref()) })
            }
            (crate::ast::Type::Array(_, len), "len") => Ok(self
                .engine
                .context
                .i64_type()
                .const_int(*len as u64, false)
                .into()),
            (crate::ast::Type::Tuple(fields), "len") => Ok(self
                .engine
                .context
                .i64_type()
                .const_int(fields.len() as u64, false)
                .into()),
            _ => Err(MatcError::CodegenError(format!(
                "Unsupported method '{}' on {:?}",
                method, receiver_mat_ty
            ))),
        }
    }
}
