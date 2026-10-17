use super::super::super::function::FunctionCompiler;
use super::super::super::util::{branch_if_unterminated, llvm_err};
use crate::ast::{EnumVariantPayload, MatchArm, MatchPattern, Type};
use crate::error::Result;
use inkwell::basic_block::BasicBlock;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, PointerValue};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_enum_match(
        &mut self,
        enum_name: &str,
        expr_mat_ty: &Type,
        compiled_expr: BasicValueEnum<'ctx>,
        arms: &[MatchArm],
        match_after: BasicBlock<'ctx>,
    ) -> Result<()> {
        let enum_llvm_ty = self.engine.llvm_type(expr_mat_ty);
        let struct_ty = enum_llvm_ty.into_struct_type();

        let alloca = self.create_entry_block_alloca(struct_ty.into(), "match_enum_tmp")?;
        llvm_err(self.engine.builder.build_store(alloca, compiled_expr))?;
        let tag_val = self.load_tag_i32(struct_ty, alloca)?;
        let enum_def = self.symbol_table.lookup_enum(enum_name).cloned();

        for arm in arms {
            let saved_vars = self.push_scope();

            let arm_block = self
                .engine
                .context
                .append_basic_block(self.fn_value, "match_arm");
            let next_arm_block = self
                .engine
                .context
                .append_basic_block(self.fn_value, "match_next_arm");

            let variant_name = match &arm.pattern {
                MatchPattern::EnumUnit { variant, .. } => Some(variant.clone()),
                MatchPattern::EnumTuple { variant, .. } => Some(variant.clone()),
                MatchPattern::EnumStruct { variant, .. } => Some(variant.clone()),
                _ => None,
            };

            if let (Some(v_name), Some(e_def)) = (variant_name, &enum_def) {
                let short_v_name = v_name.split("::").last().unwrap_or(&v_name);
                if let Some((variant_idx, variant_def)) = e_def
                    .variants
                    .iter()
                    .enumerate()
                    .find(|(_, v)| v.name == short_v_name || v.name == v_name)
                {
                    self.emit_variant_branch(tag_val, variant_idx, arm_block, next_arm_block)?;

                    self.engine.builder.position_at_end(arm_block);

                    self.extract_variant_payload(&arm.pattern, variant_def, struct_ty, alloca)?;

                    for stmt in &arm.body {
                        self.compile_statement(stmt)?;
                    }

                    self.pop_scope(saved_vars);
                    branch_if_unterminated(self.engine, match_after)?;
                    self.engine.builder.position_at_end(next_arm_block);
                } else {
                    self.pop_scope(saved_vars);
                }
            } else {
                llvm_err(self.engine.builder.build_unconditional_branch(arm_block))?;
                self.engine.builder.position_at_end(arm_block);
                for stmt in &arm.body {
                    self.compile_statement(stmt)?;
                }
                self.pop_scope(saved_vars);
                branch_if_unterminated(self.engine, match_after)?;
                self.engine.builder.position_at_end(next_arm_block);
            }
        }

        branch_if_unterminated(self.engine, match_after)?;
        self.engine.builder.position_at_end(match_after);
        Ok(())
    }

    fn load_tag_i32(
        &self,
        struct_ty: StructType<'ctx>,
        alloca: PointerValue<'ctx>,
    ) -> Result<inkwell::values::IntValue<'ctx>> {
        let tag_ptr = llvm_err(
            self.engine
                .builder
                .build_struct_gep(struct_ty, alloca, 0, "tag_ptr"),
        )?;
        let tag_val = llvm_err(self.engine.builder.build_load(
            self.engine.context.i32_type(),
            tag_ptr,
            "tag_val",
        ))?
        .into_int_value();
        Ok(tag_val)
    }

    fn emit_variant_branch(
        &self,
        tag_val: inkwell::values::IntValue<'ctx>,
        variant_idx: usize,
        arm_block: BasicBlock<'ctx>,
        next_arm_block: BasicBlock<'ctx>,
    ) -> Result<()> {
        let expected_tag = self
            .engine
            .context
            .i32_type()
            .const_int(variant_idx as u64, false);
        let cond = llvm_err(self.engine.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            tag_val,
            expected_tag,
            "tag_cmp",
        ))?;
        llvm_err(
            self.engine
                .builder
                .build_conditional_branch(cond, arm_block, next_arm_block),
        )?;
        Ok(())
    }

    fn extract_variant_payload(
        &mut self,
        pattern: &MatchPattern,
        variant_def: &crate::semantic::symbol_table::EnumVariantSymbol,
        struct_ty: StructType<'ctx>,
        alloca: PointerValue<'ctx>,
    ) -> Result<()> {
        match pattern {
            MatchPattern::EnumUnit { .. } => {}
            MatchPattern::EnumTuple { vars, .. } => {
                self.extract_tuple_payload(vars, variant_def, struct_ty, alloca)?;
            }
            MatchPattern::EnumStruct { fields, .. } => {
                self.extract_struct_payload(fields, variant_def, struct_ty, alloca)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn extract_tuple_payload(
        &mut self,
        vars: &[String],
        variant_def: &crate::semantic::symbol_table::EnumVariantSymbol,
        struct_ty: StructType<'ctx>,
        alloca: PointerValue<'ctx>,
    ) -> Result<()> {
        let payload_buf_ptr = llvm_err(self.engine.builder.build_struct_gep(
            struct_ty,
            alloca,
            1,
            "payload_buf_ptr",
        ))?;
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

        for (index, var_name) in vars.iter().enumerate() {
            let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
                payload_struct_ty,
                cast_ptr,
                index as u32,
                &format!("{}_ptr", var_name),
            ))?;
            let field_ty = match &variant_def.payload {
                EnumVariantPayload::Tuple(tys) => tys.get(index).cloned().unwrap_or(Type::Int),
                _ => Type::Int,
            };
            let llvm_field_ty = self.engine.llvm_type(&field_ty);
            let val = llvm_err(
                self.engine
                    .builder
                    .build_load(llvm_field_ty, field_ptr, var_name),
            )?;
            let var_alloca = self.create_entry_block_alloca(llvm_field_ty, var_name)?;
            llvm_err(self.engine.builder.build_store(var_alloca, val))?;
            self.local_vars
                .insert(var_name.clone(), (var_alloca, field_ty.clone()));
            self.symbol_table.insert(var_name.clone(), field_ty, false);
        }
        Ok(())
    }

    fn extract_struct_payload(
        &mut self,
        fields: &[(String, String)],
        variant_def: &crate::semantic::symbol_table::EnumVariantSymbol,
        struct_ty: StructType<'ctx>,
        alloca: PointerValue<'ctx>,
    ) -> Result<()> {
        let payload_buf_ptr = llvm_err(self.engine.builder.build_struct_gep(
            struct_ty,
            alloca,
            1,
            "payload_buf_ptr",
        ))?;
        let payload_field_tys: Vec<_> = match &variant_def.payload {
            EnumVariantPayload::Struct(flds) => {
                flds.iter().map(|f| self.engine.llvm_type(&f.ty)).collect()
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

        if let EnumVariantPayload::Struct(flds) = &variant_def.payload {
            for (field_name, var_name) in fields {
                if let Some((index, field_def)) =
                    flds.iter().enumerate().find(|(_, f)| f.name == *field_name)
                {
                    let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
                        payload_struct_ty,
                        cast_ptr,
                        index as u32,
                        &format!("{}_ptr", var_name),
                    ))?;
                    let llvm_field_ty = self.engine.llvm_type(&field_def.ty);
                    let val = llvm_err(self.engine.builder.build_load(
                        llvm_field_ty,
                        field_ptr,
                        var_name,
                    ))?;
                    let var_alloca = self.create_entry_block_alloca(llvm_field_ty, var_name)?;
                    llvm_err(self.engine.builder.build_store(var_alloca, val))?;
                    self.local_vars
                        .insert(var_name.clone(), (var_alloca, field_def.ty.clone()));
                    self.symbol_table
                        .insert(var_name.clone(), field_def.ty.clone(), false);
                }
            }
        }
        Ok(())
    }
}
