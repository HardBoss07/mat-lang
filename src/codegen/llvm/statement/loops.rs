//! Structured loop statement code generation.
//!
//! Compiles standard `fori` (C-style counting) loops and `for ... in` iterable
//! loops over arrays and vectors into LLVM basic blocks.

use super::super::function::{FunctionCompiler, LoopBlocks};
use super::super::util::{branch_if_unterminated, llvm_err};
use crate::ast::{Expression, Statement, Type};
use crate::error::{MatcError, Result};
use inkwell::basic_block::BasicBlock;
use inkwell::values::{AsValueRef, BasicValueEnum, IntValue, PointerValue};

/// Quadruple of basic blocks managing loop entry, iteration, stepping, and exit.
#[derive(Debug, Clone, Copy)]
struct LoopBlockQuad<'ctx> {
    cond: BasicBlock<'ctx>,
    body: BasicBlock<'ctx>,
    step: BasicBlock<'ctx>,
    after: BasicBlock<'ctx>,
}

impl<'a, 'ctx> FunctionCompiler<'a, 'ctx> {
    /// Creates a standard loop quad consisting of cond, body, step, and after blocks.
    fn create_loop_block_quad(&self, prefix: &str) -> LoopBlockQuad<'ctx> {
        LoopBlockQuad {
            cond: self
                .engine
                .context
                .append_basic_block(self.fn_value, &format!("{}_cond", prefix)),
            body: self
                .engine
                .context
                .append_basic_block(self.fn_value, &format!("{}_body", prefix)),
            step: self
                .engine
                .context
                .append_basic_block(self.fn_value, &format!("{}_step", prefix)),
            after: self
                .engine
                .context
                .append_basic_block(self.fn_value, &format!("{}_after", prefix)),
        }
    }

    /// Compiles a C-style indexed `fori` loop (`for init; cond; step { body }`).
    ///
    /// # Arguments
    ///
    /// * `init` - Initializer statement executed once prior to the loop.
    /// * `condition` - Boolean condition evaluated before each iteration.
    /// * `step` - Statement executed at the end of each iteration before re-evaluating condition.
    /// * `body` - Block of statements forming the loop body.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if LLVM branch construction fails.
    pub(crate) fn compile_fori_stmt(
        &mut self,
        init: &Statement,
        condition: &Expression,
        step: &Statement,
        body: &[Statement],
    ) -> Result<()> {
        let saved_vars = self.push_scope();
        self.compile_statement(init)?;

        let blocks = self.create_loop_block_quad("for");
        llvm_err(self.engine.builder.build_unconditional_branch(blocks.cond))?;

        self.engine.builder.position_at_end(blocks.cond);
        let cond_val = self.compile_expression(condition)?.into_int_value();
        llvm_err(self.engine.builder.build_conditional_branch(
            cond_val,
            blocks.body,
            blocks.after,
        ))?;

        self.execute_loop_body(blocks.body, blocks.step, blocks.after, body)?;

        self.engine.builder.position_at_end(blocks.step);
        self.compile_statement(step)?;
        llvm_err(self.engine.builder.build_unconditional_branch(blocks.cond))?;

        self.engine.builder.position_at_end(blocks.after);
        self.pop_scope(saved_vars);
        Ok(())
    }

    /// Compiles a collection iterator loop (`for item in collection { body }`).
    ///
    /// # Arguments
    ///
    /// * `var_name` - The loop variable identifier.
    /// * `iterable` - Expression yielding an array or vector.
    /// * `body` - Statements inside the loop body.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if operand type is not iterable or if LLVM construction fails.
    pub(crate) fn compile_for_in_stmt(
        &mut self,
        var_name: &str,
        iterable: &Expression,
        body: &[Statement],
    ) -> Result<()> {
        let saved_vars = self.push_scope();
        let iter_mat_ty = self
            .type_checker
            .synthesize_expr(iterable, &self.symbol_table)?;

        let iter_val = self.compile_expression(iterable)?;
        let i64_ty = self.engine.context.i64_type();

        let (elem_mat_ty, len_val, is_vector) = match iter_mat_ty {
            Type::Array(ref elem, len) => {
                (*elem.clone(), i64_ty.const_int(len as u64, false), false)
            }
            Type::Vector(ref elem) => {
                let len_fn = self.engine.module.get_function("_mat_rt_vec_len").unwrap();
                let call_res = llvm_err(self.engine.builder.build_call(
                    len_fn,
                    &[iter_val.into()],
                    "vec_len",
                ))?;
                let len_int =
                    unsafe { BasicValueEnum::new(call_res.as_value_ref()) }.into_int_value();
                (*elem.clone(), len_int, true)
            }
            _ => {
                self.pop_scope(saved_vars);
                return Err(MatcError::CodegenError(
                    "Expected array or vector type in for-in".to_string(),
                ));
            }
        };

        let iter_llvm_ty = self.engine.llvm_type(&iter_mat_ty);
        let elem_llvm_ty = self.engine.llvm_type(&elem_mat_ty);

        let iter_alloca = self.create_entry_block_alloca(iter_llvm_ty, "for_in_coll")?;
        llvm_err(self.engine.builder.build_store(iter_alloca, iter_val))?;

        let idx_alloca = self.create_entry_block_alloca(i64_ty.into(), "for_in_idx")?;
        let zero_i64 = i64_ty.const_int(0, false);
        llvm_err(self.engine.builder.build_store(idx_alloca, zero_i64))?;

        let blocks = self.create_loop_block_quad("for_in");
        llvm_err(self.engine.builder.build_unconditional_branch(blocks.cond))?;

        self.engine.builder.position_at_end(blocks.cond);
        let current_idx = llvm_err(
            self.engine
                .builder
                .build_load(i64_ty, idx_alloca, "curr_idx"),
        )?
        .into_int_value();

        let cond_val = llvm_err(self.engine.builder.build_int_compare(
            inkwell::IntPredicate::SLT,
            current_idx,
            len_val,
            "for_in_cmp",
        ))?;
        llvm_err(self.engine.builder.build_conditional_branch(
            cond_val,
            blocks.body,
            blocks.after,
        ))?;

        self.engine.builder.position_at_end(blocks.body);
        let elem_val = self.load_for_in_element(
            is_vector,
            iter_llvm_ty,
            elem_llvm_ty,
            iter_alloca,
            current_idx,
            var_name,
        )?;

        let var_alloca = self.create_entry_block_alloca(elem_llvm_ty, var_name)?;
        llvm_err(self.engine.builder.build_store(var_alloca, elem_val))?;

        self.local_vars
            .insert(var_name.to_string(), (var_alloca, elem_mat_ty.clone()));
        self.symbol_table
            .insert(var_name.to_string(), elem_mat_ty, false);

        self.execute_loop_body(blocks.body, blocks.step, blocks.after, body)?;

        self.engine.builder.position_at_end(blocks.step);
        let one_i64 = i64_ty.const_int(1, false);
        let next_idx = llvm_err(self.engine.builder.build_int_add(
            current_idx,
            one_i64,
            "next_idx",
        ))?;
        llvm_err(self.engine.builder.build_store(idx_alloca, next_idx))?;
        llvm_err(self.engine.builder.build_unconditional_branch(blocks.cond))?;

        self.engine.builder.position_at_end(blocks.after);
        self.pop_scope(saved_vars);
        Ok(())
    }

    /// Pushes loop control targets, compiles body statements, pops loop stack, and branches to step block.
    fn execute_loop_body(
        &mut self,
        body_block: BasicBlock<'ctx>,
        step_block: BasicBlock<'ctx>,
        after_block: BasicBlock<'ctx>,
        body: &[Statement],
    ) -> Result<()> {
        self.engine.builder.position_at_end(body_block);
        self.loop_stack.push(LoopBlocks {
            continue_target: step_block,
            break_target: after_block,
        });

        for stmt in body {
            self.compile_statement(stmt)?;
        }
        self.loop_stack.pop();

        branch_if_unterminated(self.engine, step_block)?;
        Ok(())
    }

    /// Loads the current indexed element from either an array buffer or vector heap buffer.
    fn load_for_in_element(
        &mut self,
        is_vector: bool,
        iter_llvm_ty: inkwell::types::BasicTypeEnum<'ctx>,
        elem_llvm_ty: inkwell::types::BasicTypeEnum<'ctx>,
        iter_alloca: PointerValue<'ctx>,
        current_idx: IntValue<'ctx>,
        var_name: &str,
    ) -> Result<BasicValueEnum<'ctx>> {
        if is_vector {
            let coll_loaded = llvm_err(self.engine.builder.build_load(
                iter_llvm_ty,
                iter_alloca,
                "vec_ptr",
            ))?;
            let get_fn = self.engine.module.get_function("_mat_rt_vec_get").unwrap();
            let call_res = llvm_err(self.engine.builder.build_call(
                get_fn,
                &[coll_loaded.into(), current_idx.into()],
                "call_vec_get",
            ))?;
            let elem_ptr =
                unsafe { BasicValueEnum::new(call_res.as_value_ref()) }.into_pointer_value();
            llvm_err(
                self.engine
                    .builder
                    .build_load(elem_llvm_ty, elem_ptr, var_name),
            )
        } else {
            let zero_i32 = self.engine.context.i32_type().const_int(0, false);
            let elem_ptr = unsafe {
                llvm_err(self.engine.builder.build_gep(
                    iter_llvm_ty,
                    iter_alloca,
                    &[zero_i32, current_idx],
                    "for_in_elem_ptr",
                ))?
            };
            llvm_err(
                self.engine
                    .builder
                    .build_load(elem_llvm_ty, elem_ptr, var_name),
            )
        }
    }
}
