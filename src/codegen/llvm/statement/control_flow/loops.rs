//! Loop-based control flow statement code generation.
//!
//! Compiles infinite `loop` statements, conditional `while` loops, and break/continue jump targets.

use super::super::super::function::{FunctionCompiler, LoopBlocks};
use super::super::super::util::{branch_if_unterminated, llvm_err};
use crate::ast::{Expression, Statement};
use crate::error::{MatcError, Result};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Compiles an infinite `loop { ... }` construct.
    ///
    /// # Arguments
    ///
    /// * `body` - Statements to execute repeatedly until a `break` is encountered.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if LLVM basic block construction fails.
    pub(crate) fn compile_loop_stmt(&mut self, body: &[Statement]) -> Result<()> {
        let loop_body = self
            .engine
            .context
            .append_basic_block(self.fn_value, "loop_body");
        let loop_after = self
            .engine
            .context
            .append_basic_block(self.fn_value, "loop_after");

        llvm_err(self.engine.builder.build_unconditional_branch(loop_body))?;

        self.engine.builder.position_at_end(loop_body);
        self.loop_stack.push(LoopBlocks {
            continue_target: loop_body,
            break_target: loop_after,
        });

        let saved_vars = self.push_scope();
        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.pop_scope(saved_vars);
        self.loop_stack.pop();

        branch_if_unterminated(self.engine, loop_body)?;

        self.engine.builder.position_at_end(loop_after);
        Ok(())
    }

    /// Compiles a `while` loop evaluated on each iteration.
    ///
    /// # Arguments
    ///
    /// * `condition` - Loop continuation condition expression.
    /// * `body` - Statements executed as long as condition evaluates to true.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if LLVM basic block or branch generation fails.
    pub(crate) fn compile_while_stmt(
        &mut self,
        condition: &Expression,
        body: &[Statement],
    ) -> Result<()> {
        let while_cond = self
            .engine
            .context
            .append_basic_block(self.fn_value, "while_cond");
        let while_body = self
            .engine
            .context
            .append_basic_block(self.fn_value, "while_body");
        let while_after = self
            .engine
            .context
            .append_basic_block(self.fn_value, "while_after");

        llvm_err(self.engine.builder.build_unconditional_branch(while_cond))?;

        self.engine.builder.position_at_end(while_cond);
        let cond_val = self.compile_expression(condition)?.into_int_value();
        llvm_err(
            self.engine
                .builder
                .build_conditional_branch(cond_val, while_body, while_after),
        )?;

        self.engine.builder.position_at_end(while_body);
        self.loop_stack.push(LoopBlocks {
            continue_target: while_cond,
            break_target: while_after,
        });

        let saved_vars = self.push_scope();
        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.pop_scope(saved_vars);
        self.loop_stack.pop();

        branch_if_unterminated(self.engine, while_cond)?;

        self.engine.builder.position_at_end(while_after);
        Ok(())
    }

    /// Compiles a `break` statement jumping to the nearest enclosing loop's exit block.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError::CodegenError`] if `break` is encountered outside of any loop.
    pub(crate) fn compile_break_stmt(&mut self) -> Result<()> {
        let loop_blocks = self
            .loop_stack
            .last()
            .ok_or_else(|| MatcError::CodegenError("break outside of loop".to_string()))?;
        let target = loop_blocks.break_target;
        llvm_err(self.engine.builder.build_unconditional_branch(target))?;

        let dead_block = self
            .engine
            .context
            .append_basic_block(self.fn_value, "after_break");
        self.engine.builder.position_at_end(dead_block);
        Ok(())
    }

    /// Compiles a `continue` statement jumping to the nearest enclosing loop's loopback block.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError::CodegenError`] if `continue` is encountered outside of any loop.
    pub(crate) fn compile_continue_stmt(&mut self) -> Result<()> {
        let loop_blocks = self
            .loop_stack
            .last()
            .ok_or_else(|| MatcError::CodegenError("continue outside of loop".to_string()))?;
        let target = loop_blocks.continue_target;
        llvm_err(self.engine.builder.build_unconditional_branch(target))?;

        let dead_block = self
            .engine
            .context
            .append_basic_block(self.fn_value, "after_continue");
        self.engine.builder.position_at_end(dead_block);
        Ok(())
    }
}
