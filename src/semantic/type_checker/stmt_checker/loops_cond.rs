//! Loop and conditional statement type checking.
//!
//! This module checks loop, while, fori, for-in, and if statements.

use crate::ast::{Expression, Statement, Type};
use crate::error::Result;
use crate::semantic::symbol_table::SymbolTable;
use crate::semantic::type_checker::TypeChecker;

impl TypeChecker {
    /// Checks a `while` loop statement and its condition.
    pub(crate) fn check_while_stmt(
        &self,
        condition: &Expression,
        body: &[Statement],
        current_return_type: &Type,
        symbols: &SymbolTable,
    ) -> Result<()> {
        let cond_ty = self.synthesize_expr(condition, symbols)?;
        if cond_ty != Type::Bool {
            return Err(self.type_error(
                format!("While condition must be bool, got {:?}", cond_ty),
                condition.span(),
            ));
        }
        symbols.push_scope();
        for inner_stmt in body {
            self.check_statement(inner_stmt, current_return_type, symbols)?;
        }
        symbols.pop_scope();
        Ok(())
    }

    /// Checks a C-style `fori` loop statement.
    pub(crate) fn check_fori_stmt(
        &self,
        init: &Statement,
        condition: &Expression,
        step: &Statement,
        body: &[Statement],
        current_return_type: &Type,
        symbols: &SymbolTable,
    ) -> Result<()> {
        symbols.push_scope();
        self.check_statement(init, current_return_type, symbols)?;
        let cond_ty = self.synthesize_expr(condition, symbols)?;
        if cond_ty != Type::Bool {
            return Err(self.type_error(
                format!("Fori condition must be bool, got {:?}", cond_ty),
                condition.span(),
            ));
        }
        self.check_statement(step, current_return_type, symbols)?;
        for inner_stmt in body {
            self.check_statement(inner_stmt, current_return_type, symbols)?;
        }
        symbols.pop_scope();
        Ok(())
    }

    /// Checks a `for-in` iterator loop statement over arrays or vectors.
    pub(crate) fn check_for_in_stmt(
        &self,
        var_name: &str,
        iterable: &Expression,
        body: &[Statement],
        current_return_type: &Type,
        symbols: &SymbolTable,
    ) -> Result<()> {
        let iter_ty = self.synthesize_expr(iterable, symbols)?;
        let elem_ty = match iter_ty {
            Type::Array(elem, _) => *elem,
            Type::Vector(elem) => *elem,
            other => {
                return Err(self.type_error(
                    format!("Cannot iterate over non-collection type {:?}", other),
                    iterable.span(),
                ));
            }
        };
        symbols.push_scope();
        symbols.insert(var_name.to_string(), elem_ty, false);
        for inner_stmt in body {
            self.check_statement(inner_stmt, current_return_type, symbols)?;
        }
        symbols.pop_scope();
        Ok(())
    }

    /// Checks an `if` / `else` conditional statement.
    pub(crate) fn check_if_stmt(
        &self,
        condition: &Expression,
        then_branch: &[Statement],
        else_branch: Option<&[Statement]>,
        current_return_type: &Type,
        symbols: &SymbolTable,
    ) -> Result<()> {
        let cond_ty = self.synthesize_expr(condition, symbols)?;
        if cond_ty != Type::Bool {
            return Err(self.type_error(
                format!("If condition must be bool, got {:?}", cond_ty),
                condition.span(),
            ));
        }
        symbols.push_scope();
        for inner_stmt in then_branch {
            self.check_statement(inner_stmt, current_return_type, symbols)?;
        }
        symbols.pop_scope();

        if let Some(else_stmts) = else_branch {
            symbols.push_scope();
            for inner_stmt in else_stmts {
                self.check_statement(inner_stmt, current_return_type, symbols)?;
            }
            symbols.pop_scope();
        }
        Ok(())
    }
}
