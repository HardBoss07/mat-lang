use super::super::function::FunctionCompiler;
use super::super::util::llvm_err;
use crate::ast::{Expression, Type};
use crate::error::{MatcError, Result};
use inkwell::values::{AsValueRef, BasicValueEnum};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_tuple_access(
        &mut self,
        expr: &Expression,
        index: usize,
    ) -> Result<BasicValueEnum<'ctx>> {
        let tuple_val = self.compile_expression(expr)?;
        if let BasicValueEnum::StructValue(struct_val) = tuple_val {
            let extracted = llvm_err(self.engine.builder.build_extract_value(
                struct_val,
                index as u32,
                "tuple_extract",
            ))?;
            Ok(extracted)
        } else {
            Err(MatcError::CodegenError(
                "Expected struct value for tuple access".to_string(),
            ))
        }
    }

    pub(crate) fn compile_array_access(
        &mut self,
        expr: &Expression,
        index: &Expression,
    ) -> Result<BasicValueEnum<'ctx>> {
        let array_mat_ty = self
            .type_checker
            .synthesize_expr(expr, &self.symbol_table)?;
        let elem_mat_ty = match &array_mat_ty {
            Type::Array(elem, _) => *elem.clone(),
            Type::Vector(elem) => *elem.clone(),
            _ => {
                return Err(MatcError::CodegenError(
                    "Expected array or vector type".to_string(),
                ));
            }
        };

        let elem_llvm_ty = self.engine.llvm_type(&elem_mat_ty);

        if let Type::Vector(_) = array_mat_ty {
            let vec_val = self.compile_expression(expr)?;
            let index_val = self.compile_expression(index)?.into_int_value();
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
            let elem_ptr = unsafe { inkwell::values::BasicValueEnum::new(call_res.as_value_ref()) }
                .into_pointer_value();
            let loaded = llvm_err(self.engine.builder.build_load(
                elem_llvm_ty,
                elem_ptr,
                "vec_elem_val",
            ))?;
            return Ok(loaded);
        }

        let array_llvm_ty = self.engine.llvm_type(&array_mat_ty);
        let array_val = self.compile_expression(expr)?;
        let index_val = self.compile_expression(index)?.into_int_value();

        let alloca = self.create_entry_block_alloca(array_llvm_ty, "arr_access_tmp")?;
        llvm_err(self.engine.builder.build_store(alloca, array_val))?;

        let zero = self.engine.context.i32_type().const_int(0, false);
        let elem_ptr = unsafe {
            llvm_err(self.engine.builder.build_gep(
                array_llvm_ty,
                alloca,
                &[zero, index_val],
                "arr_elem_gep",
            ))?
        };

        let loaded = llvm_err(self.engine.builder.build_load(
            elem_llvm_ty,
            elem_ptr,
            "arr_elem_val",
        ))?;
        Ok(loaded)
    }

    pub(crate) fn compile_field_access(
        &mut self,
        expr: &Expression,
        field: &str,
    ) -> Result<BasicValueEnum<'ctx>> {
        let struct_mat_ty = self
            .type_checker
            .synthesize_expr(expr, &self.symbol_table)?;

        let struct_name = match &struct_mat_ty {
            Type::Struct(name) | Type::Custom(name) => name.clone(),
            _ => {
                return Err(MatcError::CodegenError(
                    "Expected struct type for field access".to_string(),
                ));
            }
        };

        let struct_def = self
            .symbol_table
            .lookup_struct(&struct_name)
            .ok_or_else(|| MatcError::CodegenError(format!("Struct '{}' not found", struct_name)))?
            .clone();

        let (field_idx, (_, field_ty)) = struct_def
            .fields
            .iter()
            .enumerate()
            .find(|(_, (n, _))| n == field)
            .ok_or_else(|| {
                MatcError::CodegenError(format!("Field '{}' not found on '{}'", field, struct_name))
            })?;

        let struct_llvm_ty = self.engine.llvm_type(&struct_mat_ty);
        let field_llvm_ty = self.engine.llvm_type(field_ty);

        // If the expression is an identifier, we can directly GEP into its local variable alloca
        if let Expression::Identifier(var_name, _) = expr {
            if let Some((ptr, _)) = self.local_vars.get(var_name).cloned() {
                let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
                    struct_llvm_ty.into_struct_type(),
                    ptr,
                    field_idx as u32,
                    "field_gep",
                ))?;
                let loaded = llvm_err(self.engine.builder.build_load(
                    field_llvm_ty,
                    field_ptr,
                    "field_val",
                ))?;
                return Ok(loaded);
            }
        }

        let struct_val = self.compile_expression(expr)?;
        if let BasicValueEnum::StructValue(s_val) = struct_val {
            let extracted = llvm_err(self.engine.builder.build_extract_value(
                s_val,
                field_idx as u32,
                "field_extract",
            ))?;
            Ok(extracted)
        } else {
            let alloca = self.create_entry_block_alloca(struct_llvm_ty, "struct_acc_tmp")?;
            llvm_err(self.engine.builder.build_store(alloca, struct_val))?;
            let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
                struct_llvm_ty.into_struct_type(),
                alloca,
                field_idx as u32,
                "field_gep",
            ))?;
            let loaded = llvm_err(self.engine.builder.build_load(
                field_llvm_ty,
                field_ptr,
                "field_val",
            ))?;
            Ok(loaded)
        }
    }
}
