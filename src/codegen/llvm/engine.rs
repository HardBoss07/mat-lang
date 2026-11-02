//! Primary LLVM code generation orchestrator.
//!
//! Owns the Inkwell context, module, and IR builder. Coordinates passes for
//! nominal types, global variables, function prototypes, and function body codegen.

pub mod nominal_types;
pub mod target_output;

#[allow(unused_imports)]
pub(crate) use nominal_types::*;
#[allow(unused_imports)]
pub(crate) use target_output::*;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::FileType;
use inkwell::types::BasicType;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use super::function::FunctionCompiler;
use crate::ast::{FunctionDeclaration, Item, Program, Type};
use crate::codegen::mangling::mangle_symbol;
use crate::codegen::runtime::declare_runtime_symbols;
use crate::error::{MatcError, Result};
use crate::semantic::SymbolTable;

/// Central LLVM IR compiler engine maintaining module state and nominal type tables.
pub struct CodegenEngine<'ctx> {
    /// LLVM global context.
    pub context: &'ctx Context,
    /// Current translation unit LLVM module.
    pub module: Module<'ctx>,
    /// Instruction builder.
    pub builder: Builder<'ctx>,
    /// Struct LLVM types mapped by identifier.
    pub struct_types: RefCell<HashMap<String, inkwell::types::StructType<'ctx>>>,
    /// Enum LLVM types mapped by identifier.
    pub enum_types: RefCell<HashMap<String, inkwell::types::StructType<'ctx>>>,
}

impl<'ctx> CodegenEngine<'ctx> {
    /// Constructs a new [`CodegenEngine`] within the given LLVM context.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        declare_runtime_symbols(context, &module);

        Self {
            context,
            module,
            builder,
            struct_types: RefCell::new(HashMap::new()),
            enum_types: RefCell::new(HashMap::new()),
        }
    }

    /// Compiles an entire AST [`Program`] into LLVM IR across three coordinated passes:
    /// 1. Nominal type registration and layout finalization.
    /// 2. Global variable and function prototype declarations.
    /// 3. Function body statement lowering.
    ///
    /// # Errors
    ///
    /// Returns [`MatcError`] if any function body fails lowering.
    pub fn compile_program(&self, program: &Program) -> Result<()> {
        let mut global_symbols = SymbolTable::new();

        self.register_nominal_declarations(program, &mut global_symbols);
        self.build_nominal_type_bodies(program);
        self.declare_globals_and_prototypes(program, &mut global_symbols);
        self.compile_all_function_bodies(program, &global_symbols)?;

        Ok(())
    }

    /// Pass 1: Declares global variables and adds function prototypes to the module.
    fn declare_globals_and_prototypes(&self, program: &Program, global_symbols: &mut SymbolTable) {
        for item in &program.items {
            match item {
                Item::GlobalLet(global_let) => {
                    let ty = if let Some(annotated_ty) = &global_let.ty {
                        annotated_ty.clone()
                    } else {
                        Type::Int
                    };
                    let llvm_ty = self.llvm_type(&ty);
                    let gvar = self.module.add_global(llvm_ty, None, &global_let.name);
                    gvar.set_initializer(&llvm_ty.const_zero());
                    global_symbols.insert(global_let.name.clone(), ty, global_let.is_mutable);
                }
                Item::Function(func) => {
                    let param_tys = func.params.iter().map(|p| p.ty.clone()).collect();
                    global_symbols.insert_function(
                        func.name.clone(),
                        param_tys,
                        func.return_type.clone(),
                    );
                    self.declare_function_prototype(func);
                }
                Item::Struct(_) | Item::Enum(_) | Item::Import(_) => {}
            }
        }
    }

    /// Pass 2: Lowers each declared function's body into basic blocks.
    fn compile_all_function_bodies(
        &self,
        program: &Program,
        global_symbols: &SymbolTable,
    ) -> Result<()> {
        for item in &program.items {
            if let Item::Function(func) = item {
                self.compile_function_body(func, global_symbols)?;
            }
        }
        Ok(())
    }

    /// Declares an LLVM function prototype from its AST signature.
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

    /// Lowers a single function declaration body into LLVM IR.
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
            compiler.symbol_table.insert(
                param_ast.name.clone(),
                param_ast.ty.clone(),
                param_ast.is_mutable,
            );
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

    /// Emits the module's textual LLVM IR assembly string.
    pub fn emit_llvm_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Writes the textual LLVM IR to a file on disk.
    pub fn write_llvm_ir_to_file(&self, path: &Path) -> Result<()> {
        self.module
            .print_to_file(path)
            .map_err(|e| MatcError::CodegenError(e.to_string()))
    }

    /// Writes the compiled module to a native object file.
    pub fn write_object_to_file(&self, path: &Path) -> Result<()> {
        self.write_native_file(FileType::Object, path)
    }

    /// Writes the compiled module to a native assembly file.
    pub fn write_assembly_to_file(&self, path: &Path) -> Result<()> {
        self.write_native_file(FileType::Assembly, path)
    }
}
