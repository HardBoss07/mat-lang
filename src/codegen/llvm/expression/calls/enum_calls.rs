//! Enum variant constructor call code generation.
//!
//! This module compiles calls constructing enum variant instances with tuple payloads.

use super::super::super::function::FunctionCompiler;
use super::super::super::util::llvm_err;
use crate::ast::{EnumVariantPayload, Expression, Type};
use crate::error::{MatcError, Result};
use crate::semantic::symbol_table::EnumVariantSymbol;
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Attempts to compile a call expression as an enum tuple-variant constructor.
    ///
    /// # Arguments
    ///
    /// * `callee` - The function path identifier.
    /// * `arguments` - Payload expressions for the variant.
    ///
    /// # Returns
    ///
    /// `Some(Ok(val))` if `callee` matched an enum variant, or `None` if it is not an enum constructor.
    pub(crate) fn try_compile_enum_constructor(
        &mut self,
        callee: &str,
        arguments: &[Expression],
    ) -> Option<Result<BasicValueEnum<'ctx>>> {
        let last_colon = callee.rfind("::")?;
        let enum_name = &callee[..last_colon];
        let variant_name = &callee[last_colon + 2..];

        let enum_sym = self.symbol_table.lookup_enum(enum_name).cloned()?;
        let (tag_index, variant_def) = enum_sym
            .variants
            .iter()
            .enumerate()
            .find(|(_, v)| v.name == variant_name)?;

        Some(self.emit_enum_variant_instance(enum_name, tag_index, variant_def, arguments))
    }

    /// Emits alloca, tag store, payload store, and load for an enum variant instance.
    fn emit_enum_variant_instance(
        &mut self,
        enum_name: &str,
        tag_index: usize,
        variant_def: &EnumVariantSymbol,
        arguments: &[Expression],
    ) -> Result<BasicValueEnum<'ctx>> {
        let enum_struct_ty = self.resolve_enum_llvm_type(enum_name)?;
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

        self.store_variant_payload_fields(variant_def, arguments, payload_buf_ptr)?;

        let loaded = llvm_err(self.engine.builder.build_load(
            enum_struct_ty,
            enum_alloca,
            "enum_val",
        ))?;
        Ok(loaded)
    }

    /// Resolves the LLVM struct type for a given enum name.
    fn resolve_enum_llvm_type(&self, enum_name: &str) -> Result<inkwell::types::StructType<'ctx>> {
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

    /// Stores individual argument values into the payload buffer.
    fn store_variant_payload_fields(
        &mut self,
        variant_def: &EnumVariantSymbol,
        arguments: &[Expression],
        payload_buf_ptr: inkwell::values::PointerValue<'ctx>,
    ) -> Result<()> {
        let payload_field_tys: Vec<_> = match &variant_def.payload {
            EnumVariantPayload::Tuple(tys) => {
                tys.iter().map(|t| self.engine.llvm_type(t)).collect()
            }
            _ => vec![],
        };

        if payload_field_tys.is_empty() {
            return Ok(());
        }

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

        for (idx, arg_expr) in arguments.iter().enumerate() {
            let field_mat_ty = match &variant_def.payload {
                EnumVariantPayload::Tuple(tys) => tys.get(idx).cloned().unwrap_or(Type::Int),
                _ => Type::Int,
            };
            let raw_val = self.compile_expression(arg_expr)?;
            let val =
                super::super::super::util::coerce_val_to_type(self.engine, raw_val, &field_mat_ty)?;
            let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
                payload_struct_ty,
                cast_ptr,
                idx as u32,
                "enum_field_ptr",
            ))?;
            llvm_err(self.engine.builder.build_store(field_ptr, val))?;
        }

        Ok(())
    }
}
