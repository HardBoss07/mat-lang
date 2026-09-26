use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use std::path::Path;

use super::function::FunctionCompiler;
use crate::ast::{FunctionDeclaration, Item, Program};
use crate::codegen::mangling::mangle_symbol;
use crate::codegen::runtime::declare_runtime_symbols;
use crate::error::{MatcError, Result};

pub struct CodegenEngine<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> CodegenEngine<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        declare_runtime_symbols(context, &module);

        Self {
            context,
            module,
            builder,
        }
    }

    pub fn compile_program(&self, program: &Program) -> Result<()> {
        for item in &program.items {
            match item {
                Item::Function(func) => self.compile_function(func)?,
            }
        }
        Ok(())
    }

    fn compile_function(&self, func: &FunctionDeclaration) -> Result<()> {
        let symbol_name = mangle_symbol(&func.name);
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let fn_value = self.module.add_function(&symbol_name, fn_type, None);

        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        if symbol_name == "main" {
            let init_fn = self.module.get_function("_mat_rt_init").ok_or_else(|| {
                MatcError::CodegenError("Runtime symbol _mat_rt_init not declared".to_string())
            })?;
            self.builder
                .build_call(init_fn, &[], "call_rt_init")
                .map_err(|e| MatcError::CodegenError(e.to_string()))?;
        }

        let mut compiler = FunctionCompiler::new(self, fn_value);

        for stmt in &func.body {
            compiler.compile_statement(stmt)?;
        }

        self.builder
            .build_return(Some(&i32_type.const_int(0, false)))
            .map_err(|e| MatcError::CodegenError(e.to_string()))?;

        Ok(())
    }

    pub fn emit_llvm_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn write_llvm_ir_to_file(&self, path: &Path) -> Result<()> {
        self.module
            .print_to_file(path)
            .map_err(|e| MatcError::CodegenError(e.to_string()))
    }

    pub fn write_object_to_file(&self, path: &Path) -> Result<()> {
        self.write_native_file(FileType::Object, path)
    }

    pub fn write_assembly_to_file(&self, path: &Path) -> Result<()> {
        self.write_native_file(FileType::Assembly, path)
    }

    fn write_native_file(&self, file_type: FileType, path: &Path) -> Result<()> {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| MatcError::CodegenError(e.to_string()))?;

        let triple = TargetMachine::get_default_triple();
        let target =
            Target::from_triple(&triple).map_err(|e| MatcError::CodegenError(e.to_string()))?;

        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| {
                MatcError::CodegenError("Failed to create LLVM Target Machine".to_string())
            })?;

        target_machine
            .write_to_file(&self.module, file_type, path)
            .map_err(|e| MatcError::CodegenError(e.to_string()))
    }
}
