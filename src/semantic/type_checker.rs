use super::symbol_table::SymbolTable;
use crate::ast::{Expression, Item, Program, Statement};
use crate::error::{MatcError, Result};

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check_program(&self, program: &Program, symbols: &SymbolTable) -> Result<()> {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    for stmt in &func.body {
                        self.check_statement(stmt, symbols)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn check_statement(&self, stmt: &Statement, symbols: &SymbolTable) -> Result<()> {
        match stmt {
            Statement::Expression(expr) => self.check_expression(expr, symbols),
        }
    }

    fn check_expression(&self, expr: &Expression, symbols: &SymbolTable) -> Result<()> {
        match expr {
            Expression::Call { callee, .. } => {
                if symbols.lookup(callee).is_none() {
                    return Err(MatcError::TypeError {
                        message: format!("Undefined function: {}", callee),
                    });
                }
            }
            Expression::StringLiteral(_, _) => {}
        }
        Ok(())
    }
}
