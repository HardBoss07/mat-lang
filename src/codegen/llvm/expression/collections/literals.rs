//! Tuple, array, and vector literal code generation.
//!
//! This module compiles AST literal expressions for tuples, fixed-size arrays,
//! and dynamically allocated vectors.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::Expression;
use crate::error::{MatcError, Result};
use inkwell::types::BasicType;
use inkwell::values::{AsValueRef, BasicValueEnum};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles a tuple literal into an anonymous LLVM struct instance.
    ///
    /// # Arguments
    ///
    /// * `elements` - Element expressions making up the tuple.
    ///
    /// # Returns
    ///
    /// The loaded struct value representing the tuple.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if LLVM construction fails.
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

    /// Compiles a fixed-size array literal into an LLVM array allocation.
    ///
    /// # Arguments
    ///
    /// * `elements` - The array element expressions.
    ///
    /// # Returns
    ///
    /// The loaded array value.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if the array is empty or LLVM building fails.
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

    /// Compiles a vector literal expression into a dynamically allocated vector pointer.
    ///
    /// # Arguments
    ///
    /// * `elements` - The vector initial element expressions.
    ///
    /// # Returns
    ///
    /// Pointer to the newly initialized vector runtime instance.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if type synthesis or vector building fails.
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
}
