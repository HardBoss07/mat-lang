use std::collections::HashMap;

use inkwell::values::{FunctionValue, PointerValue};

use super::engine::CodegenEngine;
use crate::ast::Type;
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
            type_checker: TypeChecker::new(),
            loop_stack: Vec::new(),
        }
    }
}
