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

    // 1. Integer bitwidth coercion (truncation or sign extension)
    if val.is_int_value() && llvm_ty.is_int_type() {
        let val_int = val.into_int_value();
        let target_int_ty = llvm_ty.into_int_type();
        let src_width = val_int.get_type().get_bit_width();
        let target_width = target_int_ty.get_bit_width();

        if src_width > target_width {
            let trunc = llvm_err(engine.builder.build_int_truncate(
                val_int,
                target_int_ty,
                "int_trunc",
            ))?;
            return Ok(trunc.into());
        } else if src_width < target_width {
            let sext = llvm_err(engine.builder.build_int_s_extend(
                val_int,
                target_int_ty,
                "int_sext",
            ))?;
            return Ok(sext.into());
        }
    }

    // 2. Array element-wise coercion (e.g., [i64; N] -> [i32; N])
    if let Type::Array(target_elem_ty, len) = mat_ty {
        if val.is_array_value() {
            let arr_val = val.into_array_value();
            let src_arr_ty = arr_val.get_type();
            let target_arr_ty = llvm_ty.into_array_type();

            if src_arr_ty != target_arr_ty {
                let src_alloca =
                    llvm_err(engine.builder.build_alloca(src_arr_ty, "coerce_src_arr"))?;
                llvm_err(engine.builder.build_store(src_alloca, arr_val))?;

                let target_alloca = llvm_err(
                    engine
                        .builder
                        .build_alloca(target_arr_ty, "coerce_target_arr"),
                )?;

                let zero = engine.context.i32_type().const_int(0, false);
                for i in 0..*len {
                    let idx_val = engine.context.i32_type().const_int(i as u64, false);

                    let src_elem_ptr = unsafe {
                        llvm_err(engine.builder.build_gep(
                            src_arr_ty,
                            src_alloca,
                            &[zero, idx_val],
                            "src_elem_gep",
                        ))?
                    };
                    let src_elem_val = llvm_err(engine.builder.build_load(
                        src_arr_ty.get_element_type(),
                        src_elem_ptr,
                        "src_elem_val",
                    ))?;

                    let coerced_elem = coerce_val_to_type(engine, src_elem_val, target_elem_ty)?;

                    let target_elem_ptr = unsafe {
                        llvm_err(engine.builder.build_gep(
                            target_arr_ty,
                            target_alloca,
                            &[zero, idx_val],
                            "target_elem_gep",
                        ))?
                    };
                    llvm_err(engine.builder.build_store(target_elem_ptr, coerced_elem))?;
                }

                let loaded_target = llvm_err(engine.builder.build_load(
                    target_arr_ty,
                    target_alloca,
                    "coerced_arr_val",
                ))?;
                return Ok(loaded_target);
            }
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
