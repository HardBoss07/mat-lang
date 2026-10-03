use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::BasicType;
use std::path::Path;

use super::function::FunctionCompiler;
use crate::ast::{FunctionDeclaration, Item, Program, Type};
use crate::codegen::mangling::mangle_symbol;
use crate::codegen::runtime::declare_runtime_symbols;
use crate::error::{MatcError, Result};
use crate::semantic::SymbolTable;

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
        let mut global_symbols = SymbolTable::new();

        // Pass 1: Declare all function prototypes in the LLVM module first
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    let param_tys = func.params.iter().map(|p| p.ty.clone()).collect();
                    global_symbols.insert_function(
                        func.name.clone(),
                        param_tys,
                        func.return_type.clone(),
                    );
                    self.declare_function_prototype(func);
                }
                Item::Import(_) => {}
            }
        }

        // Pass 2: Compile function bodies
        for item in &program.items {
            match item {
                Item::Function(func) => self.compile_function_body(func, &global_symbols)?,
                Item::Import(_) => {}
            }
        }
        Ok(())
    }

    fn declare_function_prototype(
        &self,
        func: &FunctionDeclaration,
    ) -> inkwell::values::FunctionValue<'ctx> {
        let symbol_name = mangle_symbol(&func.name);

        let param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type(&p.ty).into())
            .collect();

        let fn_type = if func.return_type == Type::Void {
            if symbol_name == "main" {
                self.context.i32_type().fn_type(&param_types, false)
            } else {
                self.context.void_type().fn_type(&param_types, false)
            }
        } else {
            let ret_llvm = self.llvm_type(&func.return_type);
            ret_llvm.fn_type(&param_types, false)
        };

        if let Some(existing) = self.module.get_function(&symbol_name) {
            existing
        } else {
            self.module.add_function(&symbol_name, fn_type, None)
        }
    }

    fn compile_function_body(
        &self,
        func: &FunctionDeclaration,
        global_symbols: &SymbolTable,
    ) -> Result<()> {
        let symbol_name = mangle_symbol(&func.name);
        let fn_value = self.module.get_function(&symbol_name).ok_or_else(|| {
            MatcError::CodegenError(format!("Function prototype '{}' not found", symbol_name))
        })?;

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

        let mut compiler = FunctionCompiler::new(
            self,
            fn_value,
            func.return_type.clone(),
            global_symbols.clone(),
        );

        for (idx, param) in fn_value.get_param_iter().enumerate() {
            let param_ast = &func.params[idx];
            let llvm_ty = self.llvm_type(&param_ast.ty);
            let alloca = compiler.create_entry_block_alloca(llvm_ty, &param_ast.name)?;
            self.builder
                .build_store(alloca, param)
                .map_err(|e| MatcError::CodegenError(e.to_string()))?;

            compiler
                .local_vars
                .insert(param_ast.name.clone(), (alloca, param_ast.ty.clone()));
            compiler
                .symbol_table
                .insert(param_ast.name.clone(), param_ast.ty.clone(), false);
        }

        for stmt in &func.body {
            compiler.compile_statement(stmt)?;
        }

        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            if func.return_type == Type::Void {
                if symbol_name == "main" {
                    self.builder
                        .build_return(Some(&self.context.i32_type().const_int(0, false)))
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }
            }
        }

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
