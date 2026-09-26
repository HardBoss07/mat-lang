use super::function::FunctionCompiler;
use crate::ast::{BinaryOp, Statement, Type};
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
            Statement::Expression(expr) => {
                self.compile_expression(expr)?;
            }
        }
        Ok(())
    }
}
