use super::engine::CodegenEngine;
use crate::ast::Type;
use inkwell::types::{BasicType, BasicTypeEnum};

impl<'ctx> CodegenEngine<'ctx> {
    pub fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Int => self.context.i64_type().into(),
            Type::I32 => self.context.i32_type().into(),
            Type::I16 => self.context.i16_type().into(),
            Type::I8 => self.context.i8_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::F32 => self.context.f32_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::String => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            Type::Tuple(elems) => {
                let llvm_elems: Vec<BasicTypeEnum<'ctx>> =
                    elems.iter().map(|t| self.llvm_type(t)).collect();
                self.context.struct_type(&llvm_elems, false).into()
            }
            Type::Array(elem_ty, len) => {
                let elem_llvm = self.llvm_type(elem_ty);
                elem_llvm.array_type(*len as u32).into()
            }
            _ => self.context.i64_type().into(),
        }
    }
}
