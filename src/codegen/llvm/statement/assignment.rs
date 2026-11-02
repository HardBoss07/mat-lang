//! Variable declaration, assignment, and mutation statement code generation.
//!
//! Compiles `let` bindings, simple assignments, compound arithmetic assignments (`+=`, `-=`, etc.),
//! and increment/decrement (`++`, `--`) operations into LLVM instructions.

use super::super::function::FunctionCompiler;
use super::super::util::{coerce_val_to_type, llvm_err};
use crate::ast::{BinaryOp, Expression, Type};
use crate::error::{MatcError, Result};
use inkwell::values::{BasicValueEnum, PointerValue};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Resolves an lvalue target string (e.g. `"x"`, `"person.age"`, or `"tuple.0"`) to its
    /// LLVM pointer and semantic [`Type`].
    ///
    /// # Arguments
    ///
    /// * `target` - Dotted path identifying the destination memory location.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError::CodegenError`] if the variable or any nested field cannot be found.
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

    /// Evaluates an expression intended for storage, handling vector constructor synthesis
    /// and type coercion.
    fn evaluate_assignment_value(
        &mut self,
        value: &Expression,
        mat_ty: &Type,
    ) -> Result<BasicValueEnum<'ctx>> {
        let raw_val = match (value, mat_ty) {
            (Expression::Call { callee, .. }, Type::Vector(elem_ty)) if callee == "Vec::new" => {
                self.compile_builtin_vec_new(value, Some(elem_ty))?
            }
            _ => self.compile_expression(value)?,
        };
        coerce_val_to_type(self.engine, raw_val, mat_ty)
    }

    /// Compiles a local `let` binding, allocating stack space and storing the evaluated initial value.
    ///
    /// # Arguments
    ///
    /// * `name` - Variable identifier.
    /// * `ty` - Explicit optional type annotation.
    /// * `value` - Initializer expression.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError`] if type synthesis or LLVM alloca/store instructions fail.
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

        let val = self.evaluate_assignment_value(value, &mat_ty)?;
        let llvm_ty = self.engine.llvm_type(&mat_ty);

        let alloca = self.create_entry_block_alloca(llvm_ty, name)?;
        llvm_err(self.engine.builder.build_store(alloca, val))?;

        self.local_vars.insert(name.to_string(), (alloca, mat_ty));
        Ok(())
    }

    /// Compiles a direct assignment into an existing variable or member location.
    ///
    /// # Arguments
    ///
    /// * `target` - Target lvalue name or dotted path.
    /// * `value` - Expression whose evaluated value is to be stored.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError`] if target resolution or LLVM store fails.
    pub(crate) fn compile_assignment_stmt(
        &mut self,
        target: &str,
        value: &Expression,
    ) -> Result<()> {
        let (ptr, mat_ty) = self.resolve_target_ptr(target)?;
        let val = self.evaluate_assignment_value(value, &mat_ty)?;

        llvm_err(self.engine.builder.build_store(ptr, val))?;
        Ok(())
    }

    /// Compiles compound assignment (`+=`, `-=`, etc.) by delegating to [`Self::compile_binary_expr`].
    ///
    /// # Arguments
    ///
    /// * `target` - Target lvalue name or dotted path.
    /// * `op` - The binary operator being applied.
    /// * `value` - Expression representing the right operand.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError`] if target resolution, operation, or LLVM store fails.
    pub(crate) fn compile_compound_assignment_stmt(
        &mut self,
        target: &str,
        op: &BinaryOp,
        value: &Expression,
    ) -> Result<()> {
        let (ptr, mat_ty) = self.resolve_target_ptr(target)?;

        let llvm_ty = self.engine.llvm_type(&mat_ty);
        let loaded = llvm_err(self.engine.builder.build_load(llvm_ty, ptr, target))?;
        let val = self.evaluate_assignment_value(value, &mat_ty)?;

        let res = self.compile_binary_expr(op, loaded, val)?;

        llvm_err(self.engine.builder.build_store(ptr, res))?;
        Ok(())
    }

    /// Compiles a post/pre-increment statement (`++`).
    ///
    /// # Arguments
    ///
    /// * `target` - Target integer variable or path to increment.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError`] if target resolution or integer addition fails.
    pub(crate) fn compile_increment_stmt(&mut self, target: &str) -> Result<()> {
        self.apply_delta_to_target(target, 1)
    }

    /// Compiles a post/pre-decrement statement (`--`).
    ///
    /// # Arguments
    ///
    /// * `target` - Target integer variable or path to decrement.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError`] if target resolution or integer subtraction fails.
    pub(crate) fn compile_decrement_stmt(&mut self, target: &str) -> Result<()> {
        self.apply_delta_to_target(target, -1)
    }

    /// Applies a constant delta (+1 or -1) to an integer target.
    fn apply_delta_to_target(&mut self, target: &str, delta: i64) -> Result<()> {
        let (ptr, ty) = self.resolve_target_ptr(target)?;
        let llvm_ty = self.engine.llvm_type(&ty);
        let loaded =
            llvm_err(self.engine.builder.build_load(llvm_ty, ptr, target))?.into_int_value();

        let delta_val = loaded.get_type().const_int(delta.unsigned_abs(), false);
        let updated = if delta >= 0 {
            llvm_err(self.engine.builder.build_int_add(loaded, delta_val, "inc"))?
        } else {
            llvm_err(self.engine.builder.build_int_sub(loaded, delta_val, "dec"))?
        };

        llvm_err(self.engine.builder.build_store(ptr, updated))?;
        Ok(())
    }
}
