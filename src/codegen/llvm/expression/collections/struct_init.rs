//! Struct and struct-variant initialization code generation.
//!
//! This module compiles expressions initializing struct instances and enum variants with struct payloads.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::Expression;
use crate::error::{MatcError, Result};
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles a struct initialization or enum struct-variant initialization expression.
    ///
    /// # Arguments
    ///
    /// * `name` - The struct or qualified enum variant name.
    /// * `fields` - The field name and value expression pairs.
    ///
    /// # Returns
    ///
    /// The initialized struct or enum instance value.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if LLVM construction fails.
    pub(crate) fn compile_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Expression)],
    ) -> Result<BasicValueEnum<'ctx>> {
        if let Some(enum_res) = self.try_compile_enum_struct_init(name, fields)? {
            return Ok(enum_res);
        }

        self.compile_regular_struct_init(name, fields)
    }

    /// Attempts to compile a qualified name as an enum struct-variant initialization.
    fn try_compile_enum_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Expression)],
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        let last_colon = match name.rfind("::") {
            Some(idx) => idx,
            None => return Ok(None),
        };

        let enum_name = &name[..last_colon];
        let variant_name = &name[last_colon + 2..];

        let enum_sym = match self.symbol_table.lookup_enum(enum_name).cloned() {
            Some(sym) => sym,
            None => return Ok(None),
        };

        let (tag_index, variant_def) = match enum_sym
            .variants
            .iter()
            .enumerate()
            .find(|(_, v)| v.name == variant_name)
        {
            Some(found) => found,
            None => return Ok(None),
        };

        let struct_fields = match &variant_def.payload {
            crate::ast::EnumVariantPayload::Struct(sf) => sf,
            _ => return Ok(None),
        };

        let loaded =
            self.emit_enum_struct_variant_instance(enum_name, tag_index, struct_fields, fields)?;
        Ok(Some(loaded))
    }

    /// Emits alloca, tag store, and struct payload store for an enum struct-variant.
    fn emit_enum_struct_variant_instance(
        &mut self,
        enum_name: &str,
        tag_index: usize,
        struct_fields: &[crate::ast::StructField],
        fields: &[(String, Expression)],
    ) -> Result<BasicValueEnum<'ctx>> {
        let enum_struct_ty = self.resolve_enum_struct_llvm_type(enum_name)?;
        let enum_alloca = self.create_entry_block_alloca(enum_struct_ty.into(), "enum_tmp")?;

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

        self.store_enum_struct_payload_fields(struct_fields, fields, payload_buf_ptr)?;

        let loaded = llvm_err(self.engine.builder.build_load(
            enum_struct_ty,
            enum_alloca,
            "enum_val",
        ))?;
        Ok(loaded)
    }

    /// Resolves the LLVM struct type for an enum definition.
    fn resolve_enum_struct_llvm_type(
        &self,
        enum_name: &str,
    ) -> Result<inkwell::types::StructType<'ctx>> {
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
        enum_struct_ty.ok_or_else(|| {
            MatcError::CodegenError(format!(
                "LLVM struct type for enum '{}' not found",
                enum_name
            ))
        })
    }

    /// Stores struct fields into the enum variant's payload buffer.
    fn store_enum_struct_payload_fields(
        &mut self,
        struct_fields: &[crate::ast::StructField],
        fields: &[(String, Expression)],
        payload_buf_ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<()> {
        let payload_field_tys: Vec<_> = struct_fields
            .iter()
            .map(|f| self.engine.llvm_type(&f.ty))
            .collect();
        let payload_struct_ty = self.engine.context.struct_type(&payload_field_tys, false);
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

        for (idx, field_def) in struct_fields.iter().enumerate() {
            if let Some((_, field_expr)) = fields.iter().find(|(n, _)| n == &field_def.name) {
                let raw_val = self.compile_expression(field_expr)?;
                let val = super::super::super::util::coerce_val_to_type(
                    self.engine,
                    raw_val,
                    &field_def.ty,
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
        Ok(())
    }

    /// Compiles standard non-enum struct initializations.
    fn compile_regular_struct_init(
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
            let (field_idx, field_sym) = struct_def
                .fields
                .iter()
                .enumerate()
                .find(|(_, f)| f.name == *field_name)
                .ok_or_else(|| {
                    MatcError::CodegenError(format!(
                        "Field '{}' not found in struct '{}'",
                        field_name, name
                    ))
                })?;

            let field_ty = &field_sym.ty;
            let raw_val = self.compile_expression(field_expr)?;
            let val =
                super::super::super::util::coerce_val_to_type(self.engine, raw_val, field_ty)?;

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
}
