use super::super::function::FunctionCompiler;
use super::super::util::{coerce_val_to_type, llvm_err};
use crate::ast::{BinaryOp, Expression, Type};
use crate::error::{MatcError, Result};
use inkwell::values::{BasicValueEnum, PointerValue};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    fn resolve_target_ptr(&mut self, target: &str) -> Result<(PointerValue<'ctx>, Type)> {
        let parts: Vec<&str> = target.split('.').collect();
        let base_name = parts[0];
        let (mut ptr, mut mat_ty) = self
            .local_vars
            .get(base_name)
            .ok_or_else(|| {
                MatcError::CodegenError(format!("Undefined variable in codegen: {}", base_name))
            })?
            .clone();

        for field in &parts[1..] {
            match &mat_ty {
                Type::Struct(struct_name) | Type::Custom(struct_name) => {
                    let struct_def =
                        self.symbol_table
                            .lookup_struct(struct_name)
                            .ok_or_else(|| {
                                MatcError::CodegenError(format!(
                                    "Struct '{}' not found",
                                    struct_name
                                ))
                            })?;
                    let field_idx = struct_def
                        .fields
                        .iter()
                        .position(|f| f.name == *field)
                        .ok_or_else(|| {
                            MatcError::CodegenError(format!(
                                "Field '{}' not found on struct '{}'",
                                field, struct_name
                            ))
                        })?;
                    let field_mat_ty = struct_def.fields[field_idx].ty.clone();
                    let llvm_struct_ty = self.engine.llvm_type(&mat_ty).into_struct_type();
                    ptr = llvm_err(self.engine.builder.build_struct_gep(
                        llvm_struct_ty,
                        ptr,
                        field_idx as u32,
                        &format!("{}_{}_ptr", base_name, field),
                    ))?;
                    mat_ty = field_mat_ty;
                }
                Type::Tuple(elem_types) => {
                    let idx: u32 = field.parse().map_err(|_| {
                        MatcError::CodegenError(format!("Invalid tuple index '{}'", field))
                    })?;
                    let field_mat_ty = elem_types[idx as usize].clone();
                    let llvm_tuple_ty = self.engine.llvm_type(&mat_ty).into_struct_type();
                    ptr = llvm_err(self.engine.builder.build_struct_gep(
                        llvm_tuple_ty,
                        ptr,
                        idx,
                        &format!("{}_{}_ptr", base_name, field),
                    ))?;
                    mat_ty = field_mat_ty;
                }
                _ => {
                    return Err(MatcError::CodegenError(format!(
                        "Cannot access field '{}' on non-struct type {:?}",
                        field, mat_ty
                    )));
                }
            }
        }

        Ok((ptr, mat_ty))
    }

    pub(crate) fn compile_let_stmt(
        &mut self,
        name: &str,
        ty: &Option<Type>,
        value: &Expression,
    ) -> Result<()> {
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
            .insert(name.to_string(), mat_ty.clone(), false);

        let raw_val = match (value, &mat_ty) {
            (Expression::Call { callee, .. }, Type::Vector(elem_ty)) if callee == "Vec::new" => {
                self.compile_builtin_vec_new(value, Some(elem_ty))?
            }
            _ => self.compile_expression(value)?,
        };
        let val = coerce_val_to_type(self.engine, raw_val, &mat_ty)?;
        let llvm_ty = self.engine.llvm_type(&mat_ty);

        let alloca = self.create_entry_block_alloca(llvm_ty, name)?;
        llvm_err(self.engine.builder.build_store(alloca, val))?;

        self.local_vars.insert(name.to_string(), (alloca, mat_ty));
        Ok(())
    }

    pub(crate) fn compile_assignment_stmt(
        &mut self,
        target: &str,
        value: &Expression,
    ) -> Result<()> {
        let (ptr, mat_ty) = self.resolve_target_ptr(target)?;

        let raw_val = match (value, &mat_ty) {
            (Expression::Call { callee, .. }, Type::Vector(elem_ty)) if callee == "Vec::new" => {
                self.compile_builtin_vec_new(value, Some(elem_ty))?
            }
            _ => self.compile_expression(value)?,
        };
        let val = coerce_val_to_type(self.engine, raw_val, &mat_ty)?;

        llvm_err(self.engine.builder.build_store(ptr, val))?;
        Ok(())
    }

    pub(crate) fn compile_compound_assignment_stmt(
        &mut self,
        target: &str,
        op: &BinaryOp,
        value: &Expression,
    ) -> Result<()> {
        let (ptr, mat_ty) = self.resolve_target_ptr(target)?;

        let llvm_ty = self.engine.llvm_type(&mat_ty);
        let loaded = llvm_err(self.engine.builder.build_load(llvm_ty, ptr, target))?;

        let raw_val = self.compile_expression(value)?;
        let val = coerce_val_to_type(self.engine, raw_val, &mat_ty)?;

        let res: BasicValueEnum<'ctx> = if loaded.is_int_value() && val.is_int_value() {
            let l_int = loaded.into_int_value();
            let r_int = val.into_int_value();
            match op {
                BinaryOp::Add => {
                    llvm_err(self.engine.builder.build_int_add(l_int, r_int, "addtmp"))
                }
                BinaryOp::Sub => {
                    llvm_err(self.engine.builder.build_int_sub(l_int, r_int, "subtmp"))
                }
                BinaryOp::Mul => {
                    llvm_err(self.engine.builder.build_int_mul(l_int, r_int, "multmp"))
                }
                BinaryOp::Div => llvm_err(
                    self.engine
                        .builder
                        .build_int_signed_div(l_int, r_int, "divtmp"),
                ),
                BinaryOp::Mod => llvm_err(
                    self.engine
                        .builder
                        .build_int_signed_rem(l_int, r_int, "modtmp"),
                ),
                BinaryOp::Shl => {
                    llvm_err(self.engine.builder.build_left_shift(l_int, r_int, "shltmp"))
                }
                BinaryOp::Shr => llvm_err(
                    self.engine
                        .builder
                        .build_right_shift(l_int, r_int, true, "shrtmp"),
                ),
                _ => {
                    return Err(MatcError::CodegenError(
                        "Unsupported compound assignment op".to_string(),
                    ));
                }
            }?
            .into()
        } else if loaded.is_float_value() && val.is_float_value() {
            let l_float = loaded.into_float_value();
            let r_float = val.into_float_value();
            match op {
                BinaryOp::Add => llvm_err(
                    self.engine
                        .builder
                        .build_float_add(l_float, r_float, "addtmp"),
                ),
                BinaryOp::Sub => llvm_err(
                    self.engine
                        .builder
                        .build_float_sub(l_float, r_float, "subtmp"),
                ),
                BinaryOp::Mul => llvm_err(
                    self.engine
                        .builder
                        .build_float_mul(l_float, r_float, "multmp"),
                ),
                BinaryOp::Div => llvm_err(
                    self.engine
                        .builder
                        .build_float_div(l_float, r_float, "divtmp"),
                ),
                BinaryOp::Mod => llvm_err(
                    self.engine
                        .builder
                        .build_float_rem(l_float, r_float, "modtmp"),
                ),
                _ => {
                    return Err(MatcError::CodegenError(
                        "Unsupported float compound op".to_string(),
                    ));
                }
            }?
            .into()
        } else {
            return Err(MatcError::CodegenError(
                "Mismatched types in compound assignment".to_string(),
            ));
        };

        llvm_err(self.engine.builder.build_store(ptr, res))?;
        Ok(())
    }

    pub(crate) fn compile_increment_stmt(&mut self, target: &str) -> Result<()> {
        let (ptr, ty) = self.resolve_target_ptr(target)?;
        let llvm_ty = self.engine.llvm_type(&ty);
        let loaded =
            llvm_err(self.engine.builder.build_load(llvm_ty, ptr, target))?.into_int_value();

        let one = loaded.get_type().const_int(1, false);
        let inc = llvm_err(self.engine.builder.build_int_add(loaded, one, "inc"))?;

        llvm_err(self.engine.builder.build_store(ptr, inc))?;
        Ok(())
    }

    pub(crate) fn compile_decrement_stmt(&mut self, target: &str) -> Result<()> {
        let (ptr, ty) = self.resolve_target_ptr(target)?;
        let llvm_ty = self.engine.llvm_type(&ty);
        let loaded =
            llvm_err(self.engine.builder.build_load(llvm_ty, ptr, target))?.into_int_value();

        let one = loaded.get_type().const_int(1, false);
        let dec = llvm_err(self.engine.builder.build_int_sub(loaded, one, "dec"))?;

        llvm_err(self.engine.builder.build_store(ptr, dec))?;
        Ok(())
    }
}
