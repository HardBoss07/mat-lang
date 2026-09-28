use inkwell::builder::BuilderError;
use inkwell::values::BasicValueEnum;

use crate::ast::Type;
use crate::codegen::llvm::engine::CodegenEngine;
use crate::error::{MatcError, Result};

pub fn llvm_err<T>(result: std::result::Result<T, BuilderError>) -> Result<T> {
    result.map_err(|e| MatcError::CodegenError(e.to_string()))
}

pub fn coerce_val_to_type<'ctx>(
    engine: &CodegenEngine<'ctx>,
    val: BasicValueEnum<'ctx>,
    mat_ty: &Type,
) -> Result<BasicValueEnum<'ctx>> {
    let llvm_ty = engine.llvm_type(mat_ty);
    if val.is_int_value() && llvm_ty.is_int_type() {
        let val_int = val.into_int_value();
        let target_int_ty = llvm_ty.into_int_type();
        let src_width = val_int.get_type().get_bit_width();
        let target_width = target_int_ty.get_bit_width();

        if src_width > target_width {
            let trunc = engine
                .builder
                .build_int_truncate(val_int, target_int_ty, "int_trunc")
                .map_err(|e| MatcError::CodegenError(e.to_string()))?;
            return Ok(trunc.into());
        } else if src_width < target_width {
            let sext = engine
                .builder
                .build_int_s_extend(val_int, target_int_ty, "int_sext")
                .map_err(|e| MatcError::CodegenError(e.to_string()))?;
            return Ok(sext.into());
        }
    }
    Ok(val)
}

pub fn branch_if_unterminated<'ctx>(
    engine: &CodegenEngine<'ctx>,
    target: inkwell::basic_block::BasicBlock<'ctx>,
) -> Result<()> {
    if let Some(current_block) = engine.builder.get_insert_block() {
        if current_block.get_terminator().is_none() {
            engine
                .builder
                .build_unconditional_branch(target)
                .map_err(|e| MatcError::CodegenError(e.to_string()))?;
        }
    }
    Ok(())
}
