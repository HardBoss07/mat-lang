use super::super::function::{FunctionCompiler, LoopBlocks};
use super::super::util::{branch_if_unterminated, coerce_val_to_type, llvm_err};
use crate::ast::{Expression, Statement};
use crate::error::{MatcError, Result};

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
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

        let if_else = if else_branch.is_some() {
            Some(
                self.engine
                    .context
                    .append_basic_block(self.fn_value, "if_else"),
            )
        } else {
            None
        };

        let false_target = if_else.unwrap_or(if_after);

        llvm_err(
            self.engine
                .builder
                .build_conditional_branch(cond_val, if_then, false_target),
        )?;

        self.engine.builder.position_at_end(if_then);
        self.symbol_table.push_scope();
        for stmt in then_branch {
            self.compile_statement(stmt)?;
        }
        self.symbol_table.pop_scope();
        branch_if_unterminated(self.engine, if_after)?;

        if let (Some(else_block), Some(else_stmts)) = (if_else, else_branch) {
            self.engine.builder.position_at_end(else_block);
            self.symbol_table.push_scope();
            for stmt in else_stmts {
                self.compile_statement(stmt)?;
            }
            self.symbol_table.pop_scope();
            branch_if_unterminated(self.engine, if_after)?;
        }

        self.engine.builder.position_at_end(if_after);
        Ok(())
    }

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

        self.symbol_table.push_scope();
        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.symbol_table.pop_scope();
        self.loop_stack.pop();

        branch_if_unterminated(self.engine, loop_body)?;

        self.engine.builder.position_at_end(loop_after);
        Ok(())
    }

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

        self.symbol_table.push_scope();
        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.symbol_table.pop_scope();
        self.loop_stack.pop();

        branch_if_unterminated(self.engine, while_cond)?;

        self.engine.builder.position_at_end(while_after);
        Ok(())
    }

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
