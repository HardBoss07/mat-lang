//! Expression compilation dispatch.
//!
//! This module is the central dispatch point for compiling all AST expression
//! nodes into LLVM IR values. Literal values are handled inline, while
//! complex expression forms delegate to specialized submodules.

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
    /// Compiles an AST expression node into an LLVM IR value.
    ///
    /// Dispatches to the appropriate handler based on the expression variant:
    /// literals are compiled inline, while identifiers, binary operations,
    /// collections, calls, and access patterns delegate to submodule methods.
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
            Expression::Identifier(name, _) => self.compile_identifier(name),
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

    /// Compiles an identifier expression into an LLVM IR value.
    ///
    /// First checks local variables, then falls back to scoped enum variant
    /// resolution (e.g. `Color::Red`).
    fn compile_identifier(&mut self, name: &str) -> Result<BasicValueEnum<'ctx>> {
        if let Some((ptr, ty)) = self.local_vars.get(name) {
            let llvm_ty = self.engine.llvm_type(ty);
            let loaded = llvm_err(self.engine.builder.build_load(llvm_ty, *ptr, name))?;
            return Ok(loaded);
        }

        if let Some(last_colon) = name.rfind("::") {
            let enum_name = &name[..last_colon];
            let variant_name = &name[last_colon + 2..];
            if let Some(enum_sym) = self.symbol_table.lookup_enum(enum_name)
                && enum_sym.variants.iter().any(|v| v.name == variant_name)
            {
                return self.compile_enum_init(Some(enum_name), variant_name, None);
            }
        }

        Err(MatcError::CodegenError(format!(
            "Undefined variable in codegen: {}",
            name
        )))
    }

    /// Compiles an enum variant initialization expression into LLVM IR.
    ///
    /// Allocates the enum struct, writes the discriminant tag at field 0,
    /// and optionally stores the payload at field 1 via
    /// [`store_enum_payload`](Self::store_enum_payload).
    fn compile_enum_init(
        &mut self,
        enum_name: Option<&str>,
        variant_name: &str,
        payload: Option<&Expression>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let effective_name = enum_name.unwrap_or("");
        let enum_struct_ty = self.resolve_enum_llvm_type(effective_name)?;

        let enum_alloca = self.create_entry_block_alloca(enum_struct_ty.into(), "enum_tmp")?;

        let tag_index = self.resolve_variant_tag(effective_name, variant_name);
        let tag_val = self.engine.context.i32_type().const_int(tag_index, false);
        let tag_ptr = llvm_err(self.engine.builder.build_struct_gep(
            enum_struct_ty,
            enum_alloca,
            0,
            "enum_tag_ptr",
        ))?;
        llvm_err(self.engine.builder.build_store(tag_ptr, tag_val))?;

        if let Some(payload_expr) = payload {
            self.store_enum_payload(enum_struct_ty, enum_alloca, payload_expr)?;
        }

        let loaded = llvm_err(self.engine.builder.build_load(
            enum_struct_ty,
            enum_alloca,
            "enum_val",
        ))?;
        Ok(loaded)
    }

    /// Looks up the variant's positional tag index within its enum definition.
    ///
    /// Returns `0` if the enum or variant is not found in the symbol table.
    fn resolve_variant_tag(&self, enum_name: &str, variant_name: &str) -> u64 {
        self.symbol_table
            .lookup_enum(enum_name)
            .and_then(|sym| sym.variants.iter().position(|v| v.name == variant_name))
            .unwrap_or(0) as u64
    }

    /// Stores a compiled payload expression into the enum struct's payload
    /// buffer at field index 1.
    ///
    /// The payload buffer pointer is bitcast to a generic pointer and the value
    /// is written through a single-field wrapper struct GEP.
    fn store_enum_payload(
        &mut self,
        enum_struct_ty: inkwell::types::StructType<'ctx>,
        enum_alloca: inkwell::values::PointerValue<'ctx>,
        payload_expr: &Expression,
    ) -> Result<()> {
        let compiled_payload = self.compile_expression(payload_expr)?;
        let payload_buf_ptr = llvm_err(self.engine.builder.build_struct_gep(
            enum_struct_ty,
            enum_alloca,
            1,
            "enum_payload_ptr",
        ))?;
        let payload_llvm_ty = compiled_payload.get_type();
        let payload_struct_ty = self.engine.context.struct_type(&[payload_llvm_ty], false);
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

        let field_ptr = llvm_err(self.engine.builder.build_struct_gep(
            payload_struct_ty,
            cast_ptr,
            0,
            "payload_field_0",
        ))?;
        llvm_err(self.engine.builder.build_store(field_ptr, compiled_payload))?;
        Ok(())
    }
}
