mod binary_ops;
mod expr_checker;
mod stmt_checker;

use super::symbol_table::SymbolTable;
use crate::ast::{Item, Program};
use crate::error::Result;

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check_program(&self, program: &Program, symbols: &mut SymbolTable) -> Result<()> {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    let param_tys = func.params.iter().map(|p| p.ty.clone()).collect();
                    symbols.insert_function(func.name.clone(), param_tys, func.return_type.clone());
                }
            }
        }

        for item in &program.items {
            match item {
                Item::Function(func) => {
                    symbols.push_scope();
                    for param in &func.params {
                        symbols.insert(param.name.clone(), param.ty.clone(), false);
                    }
                    for stmt in &func.body {
                        self.check_statement(stmt, &func.return_type, symbols)?;
                    }
                    symbols.pop_scope();
                }
            }
        }
        Ok(())
    }
}
