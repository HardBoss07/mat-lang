pub mod symbol_table;
pub mod type_checker;

pub use symbol_table::SymbolTable;
pub use type_checker::TypeChecker;

use crate::ast::Program;
use crate::error::Result;

pub struct SemanticAnalyzer {
    pub symbol_table: SymbolTable,
    pub type_checker: TypeChecker,
}

impl SemanticAnalyzer {
    pub fn new(file_name: &str, source: &str) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            type_checker: TypeChecker::new(file_name, source),
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<()> {
        self.type_checker
            .check_program(program, &mut self.symbol_table)
    }
}
