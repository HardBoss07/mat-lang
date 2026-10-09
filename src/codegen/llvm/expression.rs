mod access;
mod binary;
mod calls;
mod collections;

use super::function::FunctionCompiler;
use super::util::llvm_err;
use crate::ast::Expression;
use crate::error::{MatcError, Result};
use inkwell::values::BasicValueEnum;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub fn compile_expression(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expression::IntLiteral(val, _) => Ok(self
                .engine
                .context
                .i64_type()
                .const_int(*val as u64, true)
                .into()),
            Expression::FloatLiteral(val, _) => {
                Ok(self.engine.context.f64_type().const_float(*val).into())
            }
            Expression::BoolLiteral(val, _) => Ok(self
                .engine
                .context
                .bool_type()
                .const_int(*val as u64, false)
                .into()),
            Expression::StringLiteral(text, _) => {
                let global_str =
                    llvm_err(self.engine.builder.build_global_string_ptr(text, "str"))?;
                Ok(global_str.as_pointer_value().into())
            }
            Expression::Identifier(name, _) => {
                let (ptr, ty) = self.local_vars.get(name).ok_or_else(|| {
                    MatcError::CodegenError(format!("Undefined variable in codegen: {}", name))
                })?;
                let llvm_ty = self.engine.llvm_type(ty);
                let loaded = llvm_err(self.engine.builder.build_load(llvm_ty, *ptr, name))?;
                Ok(loaded)
            }
            Expression::Binary {
                op, left, right, ..
            } => {
                let left_val = self.compile_expression(left)?;
                let right_val = self.compile_expression(right)?;
                self.compile_binary_expr(op, left_val, right_val)
            }
            Expression::TupleLiteral(elements, _) => self.compile_tuple_literal(elements),
            Expression::ArrayLiteral(elements, _) => self.compile_array_literal(elements),
            Expression::VectorLiteral(elements, _) => self.compile_vector_literal(elements),
            Expression::StructInit { name, fields, .. } => self.compile_struct_init(name, fields),
            Expression::EnumInit {
                enum_name,
                variant_name,
                payload,
                ..
            } => self.compile_enum_init(enum_name.as_deref(), variant_name, payload.as_deref()),
            Expression::FieldAccess { expr, field, .. } => self.compile_field_access(expr, field),
            Expression::MethodCall {
                receiver,
                method,
                args,
                ..
            } => self.compile_method_call(receiver, method, args),
            Expression::TupleAccess { expr, index, .. } => self.compile_tuple_access(expr, *index),
            Expression::ArrayAccess { expr, index, .. } => self.compile_array_access(expr, index),
            Expression::InterpolatedString(parts, _) => self.compile_interpolated_string(parts),
            Expression::Ok(val_expr, _) => self.compile_ok_expr(val_expr),
            Expression::Err(err_expr, _) => self.compile_err_expr(err_expr),
            Expression::Call {
                callee, arguments, ..
            } => self.compile_call_expr(callee, arguments),
        }
    }

    fn compile_enum_init(
        &mut self,
        enum_name: Option<&str>,
        variant_name: &str,
        payload: Option<&Expression>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let e_name = enum_name.unwrap_or("");
        let short_e_name = e_name.split("::").last().unwrap_or(e_name);

        let map = self.engine.enum_types.borrow();
        let mut enum_struct_ty = map.get(e_name).or_else(|| map.get(short_e_name)).copied();
        if enum_struct_ty.is_none() {
            let suffix = format!("::{}", short_e_name);
            for (k, &ty) in map.iter() {
                if k.ends_with(&suffix) {
                    enum_struct_ty = Some(ty);
                    break;
                }
            }
        }
        let enum_struct_ty = enum_struct_ty.ok_or_else(|| {
            MatcError::CodegenError(format!("LLVM struct type for enum '{}' not found", e_name))
        })?;

        let enum_alloca = self.create_entry_block_alloca(enum_struct_ty.into(), "enum_tmp")?;

        let enum_symbol = self.symbol_table.lookup_enum(e_name);
        let tag_index = if let Some(sym) = enum_symbol {
            sym.variants
                .iter()
                .position(|v| v.name == variant_name)
                .unwrap_or(0) as u64
        } else {
            0
        };

        let tag_val = self.engine.context.i32_type().const_int(tag_index, false);
        let tag_ptr = llvm_err(self.engine.builder.build_struct_gep(
            enum_struct_ty,
            enum_alloca,
            0,
            "enum_tag_ptr",
        ))?;
        llvm_err(self.engine.builder.build_store(tag_ptr, tag_val))?;

        if let Some(payload_expr) = payload {
            let compiled_payload = self.compile_expression(payload_expr)?;
            let payload_buf_ptr = llvm_err(self.engine.builder.build_struct_gep(
                enum_struct_ty,
                enum_alloca,
                1,
                "enum_payload_ptr",
            ))?;
            let target_ptr_ty = self
                .engine
                .context
                .ptr_type(inkwell::AddressSpace::default());
            let cast_ptr = llvm_err(self.engine.builder.build_bit_cast(
                payload_buf_ptr,
                target_ptr_ty,
                "payload_cast",
            ))?
            .into_pointer_value();
            llvm_err(self.engine.builder.build_store(cast_ptr, compiled_payload))?;
        }

        let loaded = llvm_err(self.engine.builder.build_load(
            enum_struct_ty,
            enum_alloca,
            "enum_val",
        ))?;
        Ok(loaded)
    }
}
