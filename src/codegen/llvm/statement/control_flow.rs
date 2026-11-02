//! Control flow statement code generation.
//!
//! Compiles function returns, branching `if`/`else` structures, and lexical scope management into LLVM IR.
//! Loop execution (`loop`, `while`, `break`, `continue`) is delegated to the [`loops`] submodule.

pub mod loops;

#[allow(unused_imports)]
pub(crate) use loops::*;

use super::super::function::FunctionCompiler;
use super::super::util::{branch_if_unterminated, coerce_val_to_type, llvm_err};
use crate::ast::{Expression, Statement, Type};
use crate::error::Result;
use std::collections::HashMap;

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Pushes a new lexical scope into the symbol table and snapshots existing local variables.
    ///
    /// # Returns
    ///
    /// The saved local variable map to be restored upon popping the scope.
    pub(crate) fn push_scope(
        &mut self,
    ) -> HashMap<String, (inkwell::values::PointerValue<'ctx>, Type)> {
        self.symbol_table.push_scope();
        self.local_vars.clone()
    }

    /// Pops the top lexical scope and restores the local variable environment snapshot.
    ///
    /// # Arguments
    ///
    /// * `prev_local_vars` - The local variable state prior to entering the scope.
    pub(crate) fn pop_scope(
        &mut self,
        prev_local_vars: HashMap<String, (inkwell::values::PointerValue<'ctx>, Type)>,
    ) {
        self.symbol_table.pop_scope();
        self.local_vars = prev_local_vars;
    }

    /// Compiles a `return` statement, coercing the return value to the function's signature
    /// and emitting an unreachable basic block to absorb any dead code following the return.
    ///
    /// # Arguments
    ///
    /// * `opt_expr` - Optional return expression (empty for void functions).
    ///
    /// # Errors
    ///
    /// Returns a [`crate::error::MatcError`] if LLVM instruction creation fails.
    pub(crate) fn compile_return_stmt(&mut self, opt_expr: &Option<Expression>) -> Result<()> {
        if let Some(expr) = opt_expr {
            let raw_val = self.compile_expression(expr)?;
            let val = coerce_val_to_type(self.engine, raw_val, &self.return_type)?;
            llvm_err(self.engine.builder.build_return(Some(&val)))?;
        } else {
            llvm_err(self.engine.builder.build_return(None))?;
        }

        let dead_block = self
            .engine
            .context
            .append_basic_block(self.fn_value, "after_return");
        self.engine.builder.position_at_end(dead_block);
        Ok(())
    }

    /// Compiles an `if` or `if-else` branching statement into conditional and merge basic blocks.
    ///
    /// # Arguments
    ///
    /// * `condition` - Expression evaluating to a boolean value.
    /// * `then_branch` - Statements to execute if condition is true.
    /// * `else_branch` - Optional statements to execute if condition is false.
    ///
    /// # Errors
    ///
    /// Returns a [`crate::error::MatcError`] if LLVM branch construction fails.
    pub(crate) fn compile_if_stmt(
        &mut self,
        condition: &Expression,
        then_branch: &[Statement],
        else_branch: &Option<Vec<Statement>>,
    ) -> Result<()> {
        let cond_val = self.compile_expression(condition)?.into_int_value();

        let if_then = self
            .engine
            .context
            .append_basic_block(self.fn_value, "if_then");
        let if_after = self
            .engine
            .context
            .append_basic_block(self.fn_value, "if_after");

        let if_else = else_branch.as_ref().map(|_| {
            self.engine
                .context
                .append_basic_block(self.fn_value, "if_else")
        });

        let false_target = if_else.unwrap_or(if_after);

        llvm_err(
            self.engine
                .builder
                .build_conditional_branch(cond_val, if_then, false_target),
        )?;

        self.emit_branch_block(if_then, then_branch, if_after)?;

        if let (Some(else_block), Some(else_stmts)) = (if_else, else_branch) {
            self.emit_branch_block(else_block, else_stmts, if_after)?;
        }

        self.engine.builder.position_at_end(if_after);
        Ok(())
    }

    /// Emits statements inside a conditional branch block within an isolated scope.
    fn emit_branch_block(
        &mut self,
        block: inkwell::basic_block::BasicBlock<'ctx>,
        statements: &[Statement],
        after_block: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<()> {
        self.engine.builder.position_at_end(block);
        let saved_vars = self.push_scope();
        for stmt in statements {
            self.compile_statement(stmt)?;
        }
        self.pop_scope(saved_vars);
        branch_if_unterminated(self.engine, after_block)?;
        Ok(())
    }
}
