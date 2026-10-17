use super::engine::CodegenEngine;
use crate::ast::Type;
use inkwell::types::{BasicType, BasicTypeEnum};

impl<'ctx> CodegenEngine<'ctx> {
    pub fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Void => self.context.i32_type().into(),
            Type::Int => self.context.i64_type().into(),
            Type::I32 => self.context.i32_type().into(),
            Type::I16 => self.context.i16_type().into(),
            Type::I8 => self.context.i8_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::F32 => self.context.f32_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Char => self.context.i32_type().into(),
            Type::String => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            Type::Tuple(elems) => {
                let elem_types: Vec<BasicTypeEnum> =
                    elems.iter().map(|e| self.llvm_type(e)).collect();
                self.context.struct_type(&elem_types, false).into()
            }
            Type::Array(elem, len) => {
                let elem_llvm = self.llvm_type(elem);
                elem_llvm.array_type(*len as u32).into()
            }
            Type::Result(ok_ty, err_ty) => {
                let ok_llvm = self.llvm_type(ok_ty);
                let err_llvm = self.llvm_type(err_ty);
                let bool_ty = self.context.bool_type().into();
                self.context
                    .struct_type(&[bool_ty, ok_llvm, err_llvm], false)
                    .into()
            }
            Type::Vector(_) => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            Type::Struct(name) | Type::Custom(name) => {
                let short_name = name.split("::").last().unwrap_or(name);

                let struct_map = self.struct_types.borrow();
                if let Some(&struct_ty) =
                    struct_map.get(name).or_else(|| struct_map.get(short_name))
                {
                    return struct_ty.into();
                }
                let suffix = format!("::{}", short_name);
                for (k, &struct_ty) in struct_map.iter() {
                    if k.ends_with(&suffix) {
                        return struct_ty.into();
                    }
                }

                let enum_map = self.enum_types.borrow();
                if let Some(&enum_ty) = enum_map.get(name).or_else(|| enum_map.get(short_name)) {
                    return enum_ty.into();
                }
                for (k, &enum_ty) in enum_map.iter() {
                    if k.ends_with(&suffix) {
                        return enum_ty.into();
                    }
                }

                self.context.i64_type().into()
            }
            Type::Infer => panic!(
                "Internal Compiler Error: Unresolved Type::Infer reached LLVM codegen. Type inference failed."
            ),
        }
    }
}
