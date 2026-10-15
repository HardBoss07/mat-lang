use super::super::function::FunctionCompiler;
use super::super::util::{branch_if_unterminated, llvm_err};
use crate::ast::{Expression, MatchArm, MatchPattern, Type};
use crate::error::Result;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub(crate) fn compile_match_stmt(
        &mut self,
        expr: &Expression,
        arms: &[MatchArm],
    ) -> Result<()> {
        let expr_mat_ty = self
            .type_checker
            .synthesize_expr(expr, &self.symbol_table)?;
        let compiled_expr = self.compile_expression(expr)?;

        let match_after = self
            .engine
            .context
            .append_basic_block(self.fn_value, "match_after");

        if let Type::Result(ref ok_mat_ty, ref err_mat_ty) = expr_mat_ty {
            let result_llvm_ty = self.engine.llvm_type(&expr_mat_ty);
            let struct_ty = result_llvm_ty.into_struct_type();

            let alloca = self.create_entry_block_alloca(struct_ty.into(), "match_result_tmp")?;
            llvm_err(self.engine.builder.build_store(alloca, compiled_expr))?;

            let tag_ptr = llvm_err(
                self.engine
                    .builder
                    .build_struct_gep(struct_ty, alloca, 0, "tag_ptr"),
            )?;
            let tag_val = llvm_err(self.engine.builder.build_load(
                self.engine.context.bool_type(),
                tag_ptr,
                "tag_val",
            ))?
            .into_int_value();

            let block_ok = self
                .engine
                .context
                .append_basic_block(self.fn_value, "match_ok");
            let block_err = self
                .engine
                .context
                .append_basic_block(self.fn_value, "match_err");

            llvm_err(
                self.engine
                    .builder
                    .build_conditional_branch(tag_val, block_ok, block_err),
            )?;

            for arm in arms {
                match &arm.pattern {
                    MatchPattern::Ok(var_name) => {
                        self.engine.builder.position_at_end(block_ok);
                        let saved_vars = self.push_scope();

                        let ok_llvm_ty = self.engine.llvm_type(ok_mat_ty);
                        let ok_ptr = llvm_err(
                            self.engine
                                .builder
                                .build_struct_gep(struct_ty, alloca, 1, "ok_ptr"),
                        )?;
                        let ok_val =
                            llvm_err(self.engine.builder.build_load(ok_llvm_ty, ok_ptr, var_name))?;

                        let var_alloca = self.create_entry_block_alloca(ok_llvm_ty, var_name)?;
                        llvm_err(self.engine.builder.build_store(var_alloca, ok_val))?;

                        self.local_vars
                            .insert(var_name.clone(), (var_alloca, (**ok_mat_ty).clone()));
                        self.symbol_table
                            .insert(var_name.clone(), (**ok_mat_ty).clone(), false);

                        for stmt in &arm.body {
                            self.compile_statement(stmt)?;
                        }
                        self.pop_scope(saved_vars);
                        branch_if_unterminated(self.engine, match_after)?;
                    }
                    MatchPattern::Err(var_name) => {
                        self.engine.builder.position_at_end(block_err);
                        let saved_vars = self.push_scope();

                        let err_llvm_ty = self.engine.llvm_type(err_mat_ty);
                        let err_ptr = llvm_err(
                            self.engine
                                .builder
                                .build_struct_gep(struct_ty, alloca, 2, "err_ptr"),
                        )?;
                        let err_val = llvm_err(self.engine.builder.build_load(
                            err_llvm_ty,
                            err_ptr,
                            var_name,
                        ))?;

                        let var_alloca = self.create_entry_block_alloca(err_llvm_ty, var_name)?;
                        llvm_err(self.engine.builder.build_store(var_alloca, err_val))?;

                        self.local_vars
                            .insert(var_name.clone(), (var_alloca, (**err_mat_ty).clone()));
                        self.symbol_table
                            .insert(var_name.clone(), (**err_mat_ty).clone(), false);

                        for stmt in &arm.body {
                            self.compile_statement(stmt)?;
                        }
                        self.pop_scope(saved_vars);
                        branch_if_unterminated(self.engine, match_after)?;
                    }
                    _ => {}
                }
            }

            branch_if_unterminated(self.engine, match_after)?;
            self.engine.builder.position_at_end(match_after);
        } else if let Type::Custom(ref enum_name) | Type::Struct(ref enum_name) = expr_mat_ty {
            let enum_llvm_ty = self.engine.llvm_type(&expr_mat_ty);
            let struct_ty = enum_llvm_ty.into_struct_type();

            let alloca = self.create_entry_block_alloca(struct_ty.into(), "match_enum_tmp")?;
            llvm_err(self.engine.builder.build_store(alloca, compiled_expr))?;

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

                        llvm_err(self.engine.builder.build_conditional_branch(
                            cond,
                            arm_block,
                            next_arm_block,
                        ))?;

                        self.engine.builder.position_at_end(arm_block);

                        match &arm.pattern {
                            MatchPattern::EnumUnit { .. } => {}
                            MatchPattern::EnumTuple { vars, .. } => {
                                let payload_buf_ptr =
                                    llvm_err(self.engine.builder.build_struct_gep(
                                        struct_ty,
                                        alloca,
                                        1,
                                        "payload_buf_ptr",
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

                                    for (i, var_name) in vars.iter().enumerate() {
                                        let field_ptr =
                                            llvm_err(self.engine.builder.build_struct_gep(
                                                payload_struct_ty,
                                                cast_ptr,
                                                i as u32,
                                                &format!("{}_ptr", var_name),
                                            ))?;
                                        let field_ty = match &variant_def.payload {
                                            crate::ast::EnumVariantPayload::Tuple(tys) => {
                                                tys.get(i).cloned().unwrap_or(Type::Int)
                                            }
                                            _ => Type::Int,
                                        };
                                        let llvm_field_ty = self.engine.llvm_type(&field_ty);
                                        let val = llvm_err(self.engine.builder.build_load(
                                            llvm_field_ty,
                                            field_ptr,
                                            var_name,
                                        ))?;
                                        let var_alloca = self
                                            .create_entry_block_alloca(llvm_field_ty, var_name)?;
                                        llvm_err(self.engine.builder.build_store(var_alloca, val))?;
                                        self.local_vars.insert(
                                            var_name.clone(),
                                            (var_alloca, field_ty.clone()),
                                        );
                                        self.symbol_table.insert(var_name.clone(), field_ty, false);
                                    }
                                }
                            }
                            MatchPattern::EnumStruct { fields, .. } => {
                                let payload_buf_ptr =
                                    llvm_err(self.engine.builder.build_struct_gep(
                                        struct_ty,
                                        alloca,
                                        1,
                                        "payload_buf_ptr",
                                    ))?;
                                let payload_field_tys: Vec<_> = match &variant_def.payload {
                                    crate::ast::EnumVariantPayload::Struct(flds) => {
                                        flds.iter().map(|f| self.engine.llvm_type(&f.ty)).collect()
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

                                    if let crate::ast::EnumVariantPayload::Struct(flds) =
                                        &variant_def.payload
                                    {
                                        for (field_name, var_name) in fields {
                                            if let Some((i, field_def)) = flds
                                                .iter()
                                                .enumerate()
                                                .find(|(_, f)| f.name == *field_name)
                                            {
                                                let field_ptr = llvm_err(
                                                    self.engine.builder.build_struct_gep(
                                                        payload_struct_ty,
                                                        cast_ptr,
                                                        i as u32,
                                                        &format!("{}_ptr", var_name),
                                                    ),
                                                )?;
                                                let llvm_field_ty =
                                                    self.engine.llvm_type(&field_def.ty);
                                                let val =
                                                    llvm_err(self.engine.builder.build_load(
                                                        llvm_field_ty,
                                                        field_ptr,
                                                        var_name,
                                                    ))?;
                                                let var_alloca = self.create_entry_block_alloca(
                                                    llvm_field_ty,
                                                    var_name,
                                                )?;
                                                llvm_err(
                                                    self.engine
                                                        .builder
                                                        .build_store(var_alloca, val),
                                                )?;
                                                self.local_vars.insert(
                                                    var_name.clone(),
                                                    (var_alloca, field_def.ty.clone()),
                                                );
                                                self.symbol_table.insert(
                                                    var_name.clone(),
                                                    field_def.ty.clone(),
                                                    false,
                                                );
                                            }
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }

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
        }
        Ok(())
    }
}
