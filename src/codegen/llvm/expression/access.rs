use super::super::function::FunctionCompiler;
use super::super::util::llvm_err;
use crate::ast::{Expression, Type};
use crate::error::{MatcError, Result};
use inkwell::values::BasicValueEnum;

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
        let elem_mat_ty = match array_mat_ty {
            Type::Array(ref elem, _) => *elem.clone(),
            _ => return Err(MatcError::CodegenError("Expected array type".to_string())),
        };

        let array_llvm_ty = self.engine.llvm_type(&array_mat_ty);
        let elem_llvm_ty = self.engine.llvm_type(&elem_mat_ty);

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
}
