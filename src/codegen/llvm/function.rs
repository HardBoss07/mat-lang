use inkwell::values::{FunctionValue, PointerValue};
use std::collections::HashMap;

use super::engine::CodegenEngine;
use crate::ast::Type;
use crate::semantic::{SymbolTable, TypeChecker};

pub struct FunctionCompiler<'a, 'ctx> {
    pub engine: &'a CodegenEngine<'ctx>,
    pub fn_value: FunctionValue<'ctx>,
    pub local_vars: HashMap<String, (PointerValue<'ctx>, Type)>,
    pub symbol_table: SymbolTable,
    pub type_checker: TypeChecker,
}

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub fn new(engine: &'a CodegenEngine<'ctx>, fn_value: FunctionValue<'ctx>) -> Self {
        Self {
            engine,
            fn_value,
            local_vars: HashMap::new(),
            symbol_table: SymbolTable::new(),
            type_checker: TypeChecker::new(),
        }
    }
}
