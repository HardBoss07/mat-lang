use super::function::{FunctionCompiler, LoopBlocks};
use crate::ast::{BinaryOp, MatchPattern, Statement, Type};
use crate::error::{MatcError, Result};
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    fn coerce_val_to_type(
        &self,
        val: BasicValueEnum<'ctx>,
        mat_ty: &Type,
    ) -> Result<BasicValueEnum<'ctx>> {
        let llvm_ty = self.engine.llvm_type(mat_ty);
        if val.is_int_value() && llvm_ty.is_int_type() {
            let val_int = val.into_int_value();
            let target_int_ty = llvm_ty.into_int_type();
            let src_width = val_int.get_type().get_bit_width();
            let target_width = target_int_ty.get_bit_width();

            if src_width > target_width {
                let trunc = self
                    .engine
                    .builder
                    .build_int_truncate(val_int, target_int_ty, "int_trunc")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                return Ok(trunc.into());
            } else if src_width < target_width {
                let sext = self
                    .engine
                    .builder
                    .build_int_s_extend(val_int, target_int_ty, "int_sext")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                return Ok(sext.into());
            }
        }
        Ok(val)
    }

    pub fn compile_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Return(opt_expr, _) => {
                if let Some(expr) = opt_expr {
                    let raw_val = self.compile_expression(expr)?;
                    let val = self.coerce_val_to_type(raw_val, &self.return_type)?;
                    self.engine
                        .builder
                        .build_return(Some(&val))
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                } else {
                    self.engine
                        .builder
                        .build_return(None)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                let dead_block = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "after_return");
                self.engine.builder.position_at_end(dead_block);
            }
            Statement::Match { expr, arms, .. } => {
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

                    let alloca = self
                        .engine
                        .builder
                        .build_alloca(struct_ty, "match_result_tmp")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                    self.engine
                        .builder
                        .build_store(alloca, compiled_expr)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                    let tag_ptr = self
                        .engine
                        .builder
                        .build_struct_gep(struct_ty, alloca, 0, "tag_ptr")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                    let tag_val = self
                        .engine
                        .builder
                        .build_load(self.engine.context.bool_type(), tag_ptr, "tag_val")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                        .into_int_value();

                    let block_ok = self
                        .engine
                        .context
                        .append_basic_block(self.fn_value, "match_ok");
                    let block_err = self
                        .engine
                        .context
                        .append_basic_block(self.fn_value, "match_err");

                    self.engine
                        .builder
                        .build_conditional_branch(tag_val, block_ok, block_err)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                    for arm in arms {
                        match &arm.pattern {
                            MatchPattern::Ok(var_name) => {
                                self.engine.builder.position_at_end(block_ok);
                                self.symbol_table.push_scope();

                                let ok_llvm_ty = self.engine.llvm_type(ok_mat_ty);
                                let ok_ptr = self
                                    .engine
                                    .builder
                                    .build_struct_gep(struct_ty, alloca, 1, "ok_ptr")
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                                let ok_val = self
                                    .engine
                                    .builder
                                    .build_load(ok_llvm_ty, ok_ptr, var_name)
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                                let var_alloca = self
                                    .engine
                                    .builder
                                    .build_alloca(ok_llvm_ty, var_name)
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                                self.engine
                                    .builder
                                    .build_store(var_alloca, ok_val)
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                                self.local_vars
                                    .insert(var_name.clone(), (var_alloca, (**ok_mat_ty).clone()));
                                self.symbol_table.insert(
                                    var_name.clone(),
                                    (**ok_mat_ty).clone(),
                                    false,
                                );

                                for stmt in &arm.body {
                                    self.compile_statement(stmt)?;
                                }
                                self.symbol_table.pop_scope();

                                if self
                                    .engine
                                    .builder
                                    .get_insert_block()
                                    .unwrap()
                                    .get_terminator()
                                    .is_none()
                                {
                                    self.engine
                                        .builder
                                        .build_unconditional_branch(match_after)
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                                }
                            }
                            MatchPattern::Err(var_name) => {
                                self.engine.builder.position_at_end(block_err);
                                self.symbol_table.push_scope();

                                let err_llvm_ty = self.engine.llvm_type(err_mat_ty);
                                let err_ptr = self
                                    .engine
                                    .builder
                                    .build_struct_gep(struct_ty, alloca, 2, "err_ptr")
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                                let err_val = self
                                    .engine
                                    .builder
                                    .build_load(err_llvm_ty, err_ptr, var_name)
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                                let var_alloca = self
                                    .engine
                                    .builder
                                    .build_alloca(err_llvm_ty, var_name)
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                                self.engine
                                    .builder
                                    .build_store(var_alloca, err_val)
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                                self.local_vars
                                    .insert(var_name.clone(), (var_alloca, (**err_mat_ty).clone()));
                                self.symbol_table.insert(
                                    var_name.clone(),
                                    (**err_mat_ty).clone(),
                                    false,
                                );

                                for stmt in &arm.body {
                                    self.compile_statement(stmt)?;
                                }
                                self.symbol_table.pop_scope();

                                if self
                                    .engine
                                    .builder
                                    .get_insert_block()
                                    .unwrap()
                                    .get_terminator()
                                    .is_none()
                                {
                                    self.engine
                                        .builder
                                        .build_unconditional_branch(match_after)
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                                }
                            }
                            _ => {}
                        }
                    }

                    self.engine.builder.position_at_end(match_after);
                }
            }
            Statement::Let {
                name, ty, value, ..
            } => {
                let mat_ty = match ty {
                    Some(explicit_ty) => {
                        self.type_checker
                            .check_expr(value, explicit_ty, &self.symbol_table)?
                    }
                    None => self
                        .type_checker
                        .synthesize_expr(value, &self.symbol_table)?,
                };

                self.symbol_table
                    .insert(name.clone(), mat_ty.clone(), false);

                let raw_val = self.compile_expression(value)?;
                let val = self.coerce_val_to_type(raw_val, &mat_ty)?;
                let llvm_ty = self.engine.llvm_type(&mat_ty);

                let alloca = self
                    .engine
                    .builder
                    .build_alloca(llvm_ty, name)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine
                    .builder
                    .build_store(alloca, val)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.local_vars.insert(name.clone(), (alloca, mat_ty));
            }
            Statement::Assignment { target, value, .. } => {
                let (ptr, mat_ty) = self
                    .local_vars
                    .get(target)
                    .ok_or_else(|| {
                        MatcError::CodegenError(format!(
                            "Undefined variable in codegen: {}",
                            target
                        ))
                    })?
                    .clone();

                let raw_val = self.compile_expression(value)?;
                let val = self.coerce_val_to_type(raw_val, &mat_ty)?;

                self.engine
                    .builder
                    .build_store(ptr, val)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
            }
            Statement::CompoundAssignment {
                target, op, value, ..
            } => {
                let (ptr, mat_ty) = self
                    .local_vars
                    .get(target)
                    .ok_or_else(|| {
                        MatcError::CodegenError(format!(
                            "Undefined variable in codegen: {}",
                            target
                        ))
                    })?
                    .clone();

                let llvm_ty = self.engine.llvm_type(&mat_ty);
                let loaded = self
                    .engine
                    .builder
                    .build_load(llvm_ty, ptr, target)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let raw_val = self.compile_expression(value)?;
                let val = self.coerce_val_to_type(raw_val, &mat_ty)?;

                let res: BasicValueEnum<'ctx> = if loaded.is_int_value() && val.is_int_value() {
                    let l_int = loaded.into_int_value();
                    let r_int = val.into_int_value();
                    match op {
                        BinaryOp::Add => self.engine.builder.build_int_add(l_int, r_int, "addtmp"),
                        BinaryOp::Sub => self.engine.builder.build_int_sub(l_int, r_int, "subtmp"),
                        BinaryOp::Mul => self.engine.builder.build_int_mul(l_int, r_int, "multmp"),
                        BinaryOp::Div => self
                            .engine
                            .builder
                            .build_int_signed_div(l_int, r_int, "divtmp"),
                        BinaryOp::Mod => self
                            .engine
                            .builder
                            .build_int_signed_rem(l_int, r_int, "modtmp"),
                        BinaryOp::Shl => {
                            self.engine.builder.build_left_shift(l_int, r_int, "shltmp")
                        }
                        BinaryOp::Shr => self
                            .engine
                            .builder
                            .build_right_shift(l_int, r_int, true, "shrtmp"),
                        _ => {
                            return Err(MatcError::CodegenError(
                                "Unsupported compound assignment op".to_string(),
                            ));
                        }
                    }
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?
                    .into()
                } else if loaded.is_float_value() && val.is_float_value() {
                    let l_float = loaded.into_float_value();
                    let r_float = val.into_float_value();
                    match op {
                        BinaryOp::Add => self
                            .engine
                            .builder
                            .build_float_add(l_float, r_float, "addtmp"),
                        BinaryOp::Sub => self
                            .engine
                            .builder
                            .build_float_sub(l_float, r_float, "subtmp"),
                        BinaryOp::Mul => self
                            .engine
                            .builder
                            .build_float_mul(l_float, r_float, "multmp"),
                        BinaryOp::Div => self
                            .engine
                            .builder
                            .build_float_div(l_float, r_float, "divtmp"),
                        BinaryOp::Mod => self
                            .engine
                            .builder
                            .build_float_rem(l_float, r_float, "modtmp"),
                        _ => {
                            return Err(MatcError::CodegenError(
                                "Unsupported float compound op".to_string(),
                            ));
                        }
                    }
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?
                    .into()
                } else {
                    return Err(MatcError::CodegenError(
                        "Mismatched types in compound assignment".to_string(),
                    ));
                };

                self.engine
                    .builder
                    .build_store(ptr, res)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
            }
            Statement::Increment { target, .. } => {
                let (ptr, ty) = self.local_vars.get(target).ok_or_else(|| {
                    MatcError::CodegenError(format!("Undefined variable in codegen: {}", target))
                })?;
                let llvm_ty = self.engine.llvm_type(ty);
                let loaded = self
                    .engine
                    .builder
                    .build_load(llvm_ty, *ptr, target)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?
                    .into_int_value();

                let one = loaded.get_type().const_int(1, false);
                let inc = self
                    .engine
                    .builder
                    .build_int_add(loaded, one, "inc")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine
                    .builder
                    .build_store(*ptr, inc)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
            }
            Statement::Decrement { target, .. } => {
                let (ptr, ty) = self.local_vars.get(target).ok_or_else(|| {
                    MatcError::CodegenError(format!("Undefined variable in codegen: {}", target))
                })?;
                let llvm_ty = self.engine.llvm_type(ty);
                let loaded = self
                    .engine
                    .builder
                    .build_load(llvm_ty, *ptr, target)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?
                    .into_int_value();

                let one = loaded.get_type().const_int(1, false);
                let dec = self
                    .engine
                    .builder
                    .build_int_sub(loaded, one, "dec")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine
                    .builder
                    .build_store(*ptr, dec)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
            }
            Statement::Loop { body, .. } => {
                let loop_body = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "loop_body");
                let loop_after = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "loop_after");

                self.engine
                    .builder
                    .build_unconditional_branch(loop_body)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(loop_body);
                self.loop_stack.push(LoopBlocks {
                    continue_target: loop_body,
                    break_target: loop_after,
                });

                self.symbol_table.push_scope();
                for stmt in body {
                    self.compile_statement(stmt)?;
                }
                self.symbol_table.pop_scope();
                self.loop_stack.pop();

                if self
                    .engine
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.engine
                        .builder
                        .build_unconditional_branch(loop_body)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                self.engine.builder.position_at_end(loop_after);
            }
            Statement::While {
                condition, body, ..
            } => {
                let while_cond = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "while_cond");
                let while_body = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "while_body");
                let while_after = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "while_after");

                self.engine
                    .builder
                    .build_unconditional_branch(while_cond)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(while_cond);
                let cond_val = self.compile_expression(condition)?.into_int_value();
                self.engine
                    .builder
                    .build_conditional_branch(cond_val, while_body, while_after)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(while_body);
                self.loop_stack.push(LoopBlocks {
                    continue_target: while_cond,
                    break_target: while_after,
                });

                self.symbol_table.push_scope();
                for stmt in body {
                    self.compile_statement(stmt)?;
                }
                self.symbol_table.pop_scope();
                self.loop_stack.pop();

                if self
                    .engine
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.engine
                        .builder
                        .build_unconditional_branch(while_cond)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                self.engine.builder.position_at_end(while_after);
            }
            Statement::ForI {
                init,
                condition,
                step,
                body,
                ..
            } => {
                self.symbol_table.push_scope();
                self.compile_statement(init)?;

                let for_cond = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "for_cond");
                let for_body = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "for_body");
                let for_step = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "for_step");
                let for_after = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "for_after");

                self.engine
                    .builder
                    .build_unconditional_branch(for_cond)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(for_cond);
                let cond_val = self.compile_expression(condition)?.into_int_value();
                self.engine
                    .builder
                    .build_conditional_branch(cond_val, for_body, for_after)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(for_body);
                self.loop_stack.push(LoopBlocks {
                    continue_target: for_step,
                    break_target: for_after,
                });

                for stmt in body {
                    self.compile_statement(stmt)?;
                }
                self.loop_stack.pop();

                if self
                    .engine
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.engine
                        .builder
                        .build_unconditional_branch(for_step)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                self.engine.builder.position_at_end(for_step);
                self.compile_statement(step)?;
                self.engine
                    .builder
                    .build_unconditional_branch(for_cond)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(for_after);
                self.symbol_table.pop_scope();
            }
            Statement::ForIn {
                var_name,
                iterable,
                body,
                ..
            } => {
                let iter_mat_ty = self
                    .type_checker
                    .synthesize_expr(iterable, &self.symbol_table)?;
                let (elem_mat_ty, array_len) = match iter_mat_ty {
                    Type::Array(ref elem, len) => (*elem.clone(), len),
                    _ => {
                        return Err(MatcError::CodegenError(
                            "Expected array type in for-in".to_string(),
                        ));
                    }
                };

                let iter_val = self.compile_expression(iterable)?;
                let array_llvm_ty = self.engine.llvm_type(&iter_mat_ty);
                let elem_llvm_ty = self.engine.llvm_type(&elem_mat_ty);
                let i64_ty = self.engine.context.i64_type();

                let array_alloca = self
                    .engine
                    .builder
                    .build_alloca(array_llvm_ty, "for_in_arr")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                self.engine
                    .builder
                    .build_store(array_alloca, iter_val)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let idx_alloca = self
                    .engine
                    .builder
                    .build_alloca(i64_ty, "for_in_idx")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                let zero_i64 = i64_ty.const_int(0, false);
                self.engine
                    .builder
                    .build_store(idx_alloca, zero_i64)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let for_in_cond = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "for_in_cond");
                let for_in_body = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "for_in_body");
                let for_in_step = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "for_in_step");
                let for_in_after = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "for_in_after");

                self.engine
                    .builder
                    .build_unconditional_branch(for_in_cond)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(for_in_cond);
                let current_idx = self
                    .engine
                    .builder
                    .build_load(i64_ty, idx_alloca, "curr_idx")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?
                    .into_int_value();
                let len_val = i64_ty.const_int(array_len as u64, false);
                let cond_val = self
                    .engine
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::SLT,
                        current_idx,
                        len_val,
                        "for_in_cmp",
                    )
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                self.engine
                    .builder
                    .build_conditional_branch(cond_val, for_in_body, for_in_after)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(for_in_body);
                self.symbol_table.push_scope();

                let zero_i32 = self.engine.context.i32_type().const_int(0, false);
                let elem_ptr = unsafe {
                    self.engine
                        .builder
                        .build_gep(
                            array_llvm_ty,
                            array_alloca,
                            &[zero_i32, current_idx],
                            "for_in_elem_ptr",
                        )
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                };
                let elem_val = self
                    .engine
                    .builder
                    .build_load(elem_llvm_ty, elem_ptr, var_name)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let var_alloca = self
                    .engine
                    .builder
                    .build_alloca(elem_llvm_ty, var_name)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                self.engine
                    .builder
                    .build_store(var_alloca, elem_val)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.local_vars
                    .insert(var_name.clone(), (var_alloca, elem_mat_ty.clone()));
                self.symbol_table
                    .insert(var_name.clone(), elem_mat_ty, false);

                self.loop_stack.push(LoopBlocks {
                    continue_target: for_in_step,
                    break_target: for_in_after,
                });

                for stmt in body {
                    self.compile_statement(stmt)?;
                }
                self.loop_stack.pop();

                if self
                    .engine
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.engine
                        .builder
                        .build_unconditional_branch(for_in_step)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                self.symbol_table.pop_scope();

                self.engine.builder.position_at_end(for_in_step);
                let one_i64 = i64_ty.const_int(1, false);
                let next_idx = self
                    .engine
                    .builder
                    .build_int_add(current_idx, one_i64, "next_idx")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                self.engine
                    .builder
                    .build_store(idx_alloca, next_idx)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                self.engine
                    .builder
                    .build_unconditional_branch(for_in_cond)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(for_in_after);
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_val = self.compile_expression(condition)?.into_int_value();

                let if_then = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "if_then");
                let if_after = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "if_after");

                let if_else = if else_branch.is_some() {
                    Some(
                        self.engine
                            .context
                            .append_basic_block(self.fn_value, "if_else"),
                    )
                } else {
                    None
                };

                let false_target = if_else.unwrap_or(if_after);

                self.engine
                    .builder
                    .build_conditional_branch(cond_val, if_then, false_target)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                self.engine.builder.position_at_end(if_then);
                self.symbol_table.push_scope();
                for stmt in then_branch {
                    self.compile_statement(stmt)?;
                }
                self.symbol_table.pop_scope();

                if self
                    .engine
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.engine
                        .builder
                        .build_unconditional_branch(if_after)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                if let (Some(else_block), Some(else_stmts)) = (if_else, else_branch) {
                    self.engine.builder.position_at_end(else_block);
                    self.symbol_table.push_scope();
                    for stmt in else_stmts {
                        self.compile_statement(stmt)?;
                    }
                    self.symbol_table.pop_scope();

                    if self
                        .engine
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_terminator()
                        .is_none()
                    {
                        self.engine
                            .builder
                            .build_unconditional_branch(if_after)
                            .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                    }
                }

                self.engine.builder.position_at_end(if_after);
            }
            Statement::Break(_) => {
                let loop_blocks = self
                    .loop_stack
                    .last()
                    .ok_or_else(|| MatcError::CodegenError("break outside of loop".to_string()))?;
                let target = loop_blocks.break_target;
                self.engine
                    .builder
                    .build_unconditional_branch(target)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let dead_block = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "after_break");
                self.engine.builder.position_at_end(dead_block);
            }
            Statement::Continue(_) => {
                let loop_blocks = self.loop_stack.last().ok_or_else(|| {
                    MatcError::CodegenError("continue outside of loop".to_string())
                })?;
                let target = loop_blocks.continue_target;
                self.engine
                    .builder
                    .build_unconditional_branch(target)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let dead_block = self
                    .engine
                    .context
                    .append_basic_block(self.fn_value, "after_continue");
                self.engine.builder.position_at_end(dead_block);
            }
            Statement::Expression(expr) => {
                self.compile_expression(expr)?;
            }
        }
        Ok(())
    }
}
