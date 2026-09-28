mod assignment;
mod control_flow;
mod loops;
mod match_stmt;

use super::function::FunctionCompiler;
use crate::ast::Statement;
use crate::error::Result;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    pub fn compile_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Return(opt_expr, _) => self.compile_return_stmt(opt_expr),
            Statement::Match { expr, arms, .. } => self.compile_match_stmt(expr, arms),
            Statement::Let {
                name, ty, value, ..
            } => self.compile_let_stmt(name, ty, value),
            Statement::Assignment { target, value, .. } => {
                self.compile_assignment_stmt(target, value)
            }
            Statement::CompoundAssignment {
                target, op, value, ..
            } => self.compile_compound_assignment_stmt(target, op, value),
            Statement::Increment { target, .. } => self.compile_increment_stmt(target),
            Statement::Decrement { target, .. } => self.compile_decrement_stmt(target),
            Statement::Loop { body, .. } => self.compile_loop_stmt(body),
            Statement::While {
                condition, body, ..
            } => self.compile_while_stmt(condition, body),
            Statement::ForI {
                init,
                condition,
                step,
                body,
                ..
            } => self.compile_fori_stmt(init, condition, step, body),
            Statement::ForIn {
                var_name,
                iterable,
                body,
                ..
            } => self.compile_for_in_stmt(var_name, iterable, body),
            Statement::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => self.compile_if_stmt(condition, then_branch, else_branch),
            Statement::Break(_) => self.compile_break_stmt(),
            Statement::Continue(_) => self.compile_continue_stmt(),
            Statement::Expression(expr) => {
                self.compile_expression(expr)?;
                Ok(())
            }
        }
    }
}
