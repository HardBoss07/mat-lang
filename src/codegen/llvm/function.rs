use std::collections::HashMap;

use inkwell::types::BasicTypeEnum;
use inkwell::values::{FunctionValue, PointerValue};

use super::engine::CodegenEngine;
use super::util::llvm_err;
use crate::ast::Type;
use crate::error::{MatcError, Result};
use crate::semantic::{SymbolTable, TypeChecker};

pub struct LoopBlocks<'ctx> {
    pub continue_target: inkwell::basic_block::BasicBlock<'ctx>,
    pub break_target: inkwell::basic_block::BasicBlock<'ctx>,
}

pub struct FunctionCompiler<'a, 'ctx> {
    pub engine: &'a CodegenEngine<'ctx>,
    pub fn_value: FunctionValue<'ctx>,
    pub return_type: Type,
    pub local_vars: HashMap<String, (PointerValue<'ctx>, Type)>,
    pub symbol_table: SymbolTable,
    pub type_checker: TypeChecker,
    pub loop_stack: Vec<LoopBlocks<'ctx>>,
}

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub fn new(
        engine: &'a CodegenEngine<'ctx>,
        fn_value: FunctionValue<'ctx>,
        return_type: Type,
        symbol_table: SymbolTable,
    ) -> Self {
        Self {
            engine,
            fn_value,
            return_type,
            local_vars: HashMap::new(),
            symbol_table,
            type_checker: TypeChecker::empty(),
            loop_stack: Vec::new(),
        }
    }

    pub fn create_entry_block_alloca(
        &self,
        ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> Result<PointerValue<'ctx>> {
        let builder = self.engine.context.create_builder();
        let entry_block = self
            .fn_value
            .get_first_basic_block()
            .ok_or_else(|| MatcError::CodegenError("Function entry block not found".to_string()))?;

        if let Some(first_instr) = entry_block.get_first_instruction() {
            builder.position_before(&first_instr);
        } else {
            builder.position_at_end(entry_block);
        }

        llvm_err(builder.build_alloca(ty, name))
    }
}
