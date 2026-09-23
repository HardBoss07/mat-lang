use super::symbol_table::SymbolTable;
use crate::ast::Statement;
use crate::error::Result;

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check_statement(&self, _stmt: &Statement, _symbols: &mut SymbolTable) -> Result<()> {
        Ok(())
    }
}
