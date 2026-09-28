use super::super::symbol_table::SymbolTable;
use super::TypeChecker;
use crate::ast::{MatchPattern, Statement, Type};
use crate::error::Result;

impl TypeChecker {
    pub(crate) fn check_statement(
        &self,
        stmt: &Statement,
        current_return_type: &Type,
        symbols: &mut SymbolTable,
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
                let expr_ty = self.synthesize_expr(expr, symbols)?;
                for arm in arms {
                    symbols.push_scope();
                    match (&arm.pattern, &expr_ty) {
                        (MatchPattern::Ok(var_name), Type::Result(ok_ty, _)) => {
                            symbols.insert(var_name.clone(), *ok_ty.clone(), false);
                        }
                        (MatchPattern::Err(var_name), Type::Result(_, err_ty)) => {
                            symbols.insert(var_name.clone(), *err_ty.clone(), false);
                        }
                        (MatchPattern::Wildcard, _) => {}
                        (MatchPattern::Literal(lit), target_ty) => {
                            self.check_expr(lit, target_ty, symbols)?;
                        }
                        _ => {
                            return Err(self.type_error(
                                format!(
                                    "Pattern {:?} mismatch for matched type {:?}",
                                    arm.pattern, expr_ty
                                ),
                                *span,
                            ));
                        }
                    }
                    for stmt in &arm.body {
                        self.check_statement(stmt, current_return_type, symbols)?;
                    }
                    symbols.pop_scope();
                }
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
                    None => self.synthesize_expr(value, symbols)?,
                };
                symbols.insert(name.clone(), inferred_ty, *is_mutable);
            }
            Statement::Assignment {
                target,
                value,
                span,
            } => {
                let sym = symbols
                    .lookup(target)
                    .ok_or_else(|| {
                        self.type_error(format!("Undefined variable: {}", target), *span)
                    })?
                    .clone();
                self.check_expr(value, &sym.ty, symbols)?;
            }
            Statement::CompoundAssignment {
                target,
                value,
                span,
                ..
            } => {
                let sym = symbols
                    .lookup(target)
                    .ok_or_else(|| {
                        self.type_error(format!("Undefined variable: {}", target), *span)
                    })?
                    .clone();
                self.check_expr(value, &sym.ty, symbols)?;
            }
            Statement::Increment { target, span } | Statement::Decrement { target, span } => {
                let _ = symbols.lookup(target).ok_or_else(|| {
                    self.type_error(format!("Undefined variable: {}", target), *span)
                })?;
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
            }
            Statement::ForI {
                init,
                condition,
                step,
                body,
                ..
            } => {
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
            }
            Statement::ForIn {
                var_name,
                iterable,
                body,
                ..
            } => {
                let iter_ty = self.synthesize_expr(iterable, symbols)?;
                let elem_ty = match iter_ty {
                    Type::Array(elem, _) => *elem,
                    other => {
                        return Err(self.type_error(
                            format!("Cannot iterate over non-array type {:?}", other),
                            iterable.span(),
                        ));
                    }
                };
                symbols.push_scope();
                symbols.insert(var_name.clone(), elem_ty, false);
                for inner_stmt in body {
                    self.check_statement(inner_stmt, current_return_type, symbols)?;
                }
                symbols.pop_scope();
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
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
            }
            Statement::Break(_) | Statement::Continue(_) => {}
        }
        Ok(())
    }
}
