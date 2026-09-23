pub mod symbol_table;
pub mod type_checker;

pub use symbol_table::SymbolTable;
pub use type_checker::TypeChecker;

use crate::ast::Statement;
use crate::error::Result;

pub struct SemanticAnalyzer {
    pub symbol_table: SymbolTable,
    pub type_checker: TypeChecker,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            type_checker: TypeChecker::new(),
        }
    }

    pub fn analyze(&mut self, statements: &[Statement]) -> Result<()> {
        for stmt in statements {
            self.type_checker
                .check_statement(stmt, &mut self.symbol_table)?;
        }
        Ok(())
    }
}
