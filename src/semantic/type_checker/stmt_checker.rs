//! Statement type checking implementation.
//!
//! This module coordinates type checking of return, variable declaration, assignment,
//! mutation, pattern matching, loops, and conditional control flow statements.

pub mod loops_cond;
pub mod match_checker;
pub mod target_resolver;

#[allow(unused_imports)]
pub(crate) use loops_cond::*;
#[allow(unused_imports)]
pub(crate) use match_checker::*;
#[allow(unused_imports)]
pub(crate) use target_resolver::*;

use super::super::symbol_table::SymbolTable;
use super::TypeChecker;
use crate::ast::{Statement, Type};
use crate::error::Result;

impl TypeChecker {
    /// Validates type safety and symbol bindings for a single statement.
    ///
    /// # Arguments
    ///
    /// * `stmt` - The AST statement node to check.
    /// * `current_return_type` - Expected return type of the enclosing function.
    /// * `symbols` - Active symbol table for the scope.
    ///
    /// # Returns
    ///
    /// `Ok(())` on valid statement typing.
    ///
    /// # Errors
    ///
    /// Returns a [`crate::error::MatcError`] if types do not match or an invalid construct is detected.
    pub(crate) fn check_statement(
        &self,
        stmt: &Statement,
        current_return_type: &Type,
        symbols: &SymbolTable,
    ) -> Result<()> {
        match stmt {
            Statement::Return(opt_expr, span) => match (opt_expr, current_return_type) {
                (Some(expr), target_ty) => {
                    self.check_expr(expr, target_ty, symbols)?;
                }
                (None, Type::Void) => {}
                (None, expected) => {
                    return Err(self.type_error(
                        format!(
                            "Empty return statement in function expecting return type {:?}",
                            expected
                        ),
                        *span,
                    ));
                }
            },
            Statement::Match { expr, arms, span } => {
                self.check_match_stmt(expr, arms, *span, current_return_type, symbols)?;
            }
            Statement::Let {
                name,
                is_mutable,
                ty,
                value,
                ..
            } => {
                let inferred_ty = match ty {
                    Some(target_ty) => self.check_expr(value, target_ty, symbols)?,
                    None => self.check_expr(value, &Type::Infer, symbols)?,
                };
                symbols.insert(name.clone(), inferred_ty, *is_mutable);
            }
            Statement::Assignment {
                target,
                value,
                span,
            }
            | Statement::CompoundAssignment {
                target,
                value,
                span,
                ..
            } => {
                let target_ty = self.resolve_target_type(target, symbols, *span)?;
                self.check_expr(value, &target_ty, symbols)?;
            }
            Statement::Increment { target, span } | Statement::Decrement { target, span } => {
                let _ = self.resolve_target_type(target, symbols, *span)?;
            }
            Statement::Expression(expr) => {
                self.synthesize_expr(expr, symbols)?;
            }
            Statement::Loop { body, .. } => {
                symbols.push_scope();
                for inner_stmt in body {
                    self.check_statement(inner_stmt, current_return_type, symbols)?;
                }
                symbols.pop_scope();
            }
            Statement::While {
                condition, body, ..
            } => {
                self.check_while_stmt(condition, body, current_return_type, symbols)?;
            }
            Statement::ForI {
                init,
                condition,
                step,
                body,
                ..
            } => {
                self.check_fori_stmt(init, condition, step, body, current_return_type, symbols)?;
            }
            Statement::ForIn {
                var_name,
                iterable,
                body,
                ..
            } => {
                self.check_for_in_stmt(var_name, iterable, body, current_return_type, symbols)?;
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.check_if_stmt(
                    condition,
                    then_branch,
                    else_branch.as_deref(),
                    current_return_type,
                    symbols,
                )?;
            }
            Statement::Break(_) | Statement::Continue(_) => {}
        }
        Ok(())
    }
}
