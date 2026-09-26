use super::function::FunctionCompiler;
use crate::ast::Statement;
use crate::error::{MatcError, Result};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
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

                let val = self.compile_expression(value)?;
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
                let val = self.compile_expression(value)?;
                let (ptr, _) = self.local_vars.get(target).ok_or_else(|| {
                    MatcError::CodegenError(format!("Undefined variable in codegen: {}", target))
                })?;
                self.engine
                    .builder
                    .build_store(*ptr, val)
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
