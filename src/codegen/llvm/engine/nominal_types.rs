//! Struct and enum nominal type declaration and body construction for LLVM.

use super::CodegenEngine;
use crate::ast::{Item, Program};
use crate::semantic::SymbolTable;
use inkwell::types::BasicType;

impl<'ctx> CodegenEngine<'ctx> {
    /// Registers all nominal struct and enum types as opaque LLVM struct types and records
    /// their field/variant definitions in the global symbol table.
    pub(crate) fn register_nominal_declarations(
        &self,
        program: &Program,
        global_symbols: &mut SymbolTable,
    ) {
        for item in &program.items {
            match item {
                Item::Struct(struct_decl) => {
                    let fields = struct_decl
                        .fields
                        .iter()
                        .map(|f| crate::semantic::symbol_table::StructFieldSymbol {
                            is_pub: f.is_pub,
                            name: f.name.clone(),
                            ty: f.ty.clone(),
                        })
                        .collect();
                    global_symbols.insert_struct(struct_decl.name.clone(), fields);

                    let struct_type = self.context.opaque_struct_type(&struct_decl.name);
                    self.struct_types
                        .borrow_mut()
                        .insert(struct_decl.name.clone(), struct_type);
                }
                Item::Enum(enum_decl) => {
                    let variants = enum_decl
                        .variants
                        .iter()
                        .map(|v| crate::semantic::symbol_table::EnumVariantSymbol {
                            name: v.name.clone(),
                            payload: v.payload.clone(),
                        })
                        .collect();
                    global_symbols.insert_enum(enum_decl.name.clone(), variants);

                    let enum_type = self.context.opaque_struct_type(&enum_decl.name);
                    self.enum_types
                        .borrow_mut()
                        .insert(enum_decl.name.clone(), enum_type);
                }
                _ => {}
            }
        }
    }

    /// Finalizes the memory layouts of registered struct and enum types now that all types exist.
    pub(crate) fn build_nominal_type_bodies(&self, program: &Program) {
        for item in &program.items {
            if let Item::Struct(struct_decl) = item {
                let field_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> = struct_decl
                    .fields
                    .iter()
                    .map(|f| self.llvm_type(&f.ty))
                    .collect();
                if let Some(struct_type) = self.struct_types.borrow().get(&struct_decl.name) {
                    struct_type.set_body(&field_types, false);
                }
            } else if let Item::Enum(enum_decl) = item {
                // Tagged union in LLVM: { i32 tag, [64 x i8] payload }
                let tag_type = self.context.i32_type().as_basic_type_enum();
                let payload_bytes = self.context.i8_type().array_type(64).as_basic_type_enum();
                if let Some(enum_type) = self.enum_types.borrow().get(&enum_decl.name) {
                    enum_type.set_body(&[tag_type, payload_bytes], false);
                }
            }
        }
    }
}
