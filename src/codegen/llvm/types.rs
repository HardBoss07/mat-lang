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
            Type::Custom(_) => self.context.i64_type().into(),
        }
    }
}
