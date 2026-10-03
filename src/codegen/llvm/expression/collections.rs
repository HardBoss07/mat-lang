use super::super::function::FunctionCompiler;
use super::super::util::llvm_err;
use crate::ast::Expression;
use crate::error::{MatcError, Result};
use inkwell::types::BasicType;
use inkwell::values::BasicValueEnum;

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
}
