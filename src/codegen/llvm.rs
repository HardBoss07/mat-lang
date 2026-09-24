use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};
use std::collections::HashMap;
use std::path::Path;

use crate::ast::{Expression, FunctionDeclaration, Item, Program, Statement, Type};
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

        let mut local_vars: HashMap<String, (PointerValue<'ctx>, Type)> = HashMap::new();

        for stmt in &func.body {
            match stmt {
                Statement::Let {
                    name, ty, value, ..
                } => {
                    let val = self.compile_expression(value, &local_vars)?;
                    let llvm_ty: BasicTypeEnum<'ctx> = match ty {
                        Type::Int => self.context.i64_type().into(),
                        Type::Bool => self.context.bool_type().into(),
                        Type::String => self
                            .context
                            .ptr_type(inkwell::AddressSpace::default())
                            .into(),
                        _ => self.context.i64_type().into(),
                    };

                    let alloca = self
                        .builder
                        .build_alloca(llvm_ty, name)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                    self.builder
                        .build_store(alloca, val)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                    local_vars.insert(name.clone(), (alloca, ty.clone()));
                }
                Statement::Expression(expr) => {
                    self.compile_expression(expr, &local_vars)?;
                }
            }
        }

        self.builder
            .build_return(Some(&i32_type.const_int(0, false)))
            .map_err(|e| MatcError::CodegenError(e.to_string()))?;

        Ok(())
    }

    fn compile_expression(
        &self,
        expr: &Expression,
        local_vars: &HashMap<String, (PointerValue<'ctx>, Type)>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expression::IntLiteral(val, _) => {
                Ok(self.context.i64_type().const_int(*val as u64, true).into())
            }
            Expression::BoolLiteral(val, _) => Ok(self
                .context
                .bool_type()
                .const_int(*val as u64, false)
                .into()),
            Expression::StringLiteral(text, _) => {
                let global_str = self
                    .builder
                    .build_global_string_ptr(text, "str")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(global_str.as_pointer_value().into())
            }
            Expression::Identifier(name, _) => {
                let (ptr, ty) = local_vars.get(name).ok_or_else(|| {
                    MatcError::CodegenError(format!("Undefined variable in codegen: {}", name))
                })?;

                let llvm_ty: BasicTypeEnum<'ctx> = match ty {
                    Type::Int => self.context.i64_type().into(),
                    Type::Bool => self.context.bool_type().into(),
                    Type::String => self
                        .context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(),
                    _ => self.context.i64_type().into(),
                };

                let loaded = self
                    .builder
                    .build_load(llvm_ty, *ptr, name)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::InterpolatedString(parts, _) => {
                let mut fmt_string = String::new();
                let mut args = Vec::new();

                for part in parts {
                    match part {
                        Expression::StringLiteral(s, _) => fmt_string.push_str(s),
                        other => {
                            let val = self.compile_expression(other, local_vars)?;
                            if val.is_int_value() {
                                fmt_string.push_str("%ld");
                            } else if val.is_pointer_value() {
                                fmt_string.push_str("%s");
                            }
                            args.push(val);
                        }
                    }
                }

                let printf_fn = self.module.get_function("printf").unwrap();
                let fmt_ptr = self
                    .builder
                    .build_global_string_ptr(&fmt_string, "fmt")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                    vec![fmt_ptr.as_pointer_value().into()];
                for arg in args {
                    call_args.push(arg.into());
                }

                self.builder
                    .build_call(printf_fn, &call_args, "call_printf")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                Ok(self.context.i32_type().const_int(0, false).into())
            }
            Expression::Call {
                callee, arguments, ..
            } => {
                if callee == "println" {
                    if let Some(first_arg) = arguments.first() {
                        let printf_fn = self.module.get_function("printf").unwrap();

                        match first_arg {
                            Expression::InterpolatedString(parts, _) => {
                                let mut fmt_string = String::new();
                                let mut args = Vec::new();

                                for part in parts {
                                    match part {
                                        Expression::StringLiteral(s, _) => fmt_string.push_str(s),
                                        other => {
                                            let val = self.compile_expression(other, local_vars)?;
                                            if val.is_int_value() {
                                                fmt_string.push_str("%ld");
                                            } else if val.is_pointer_value() {
                                                fmt_string.push_str("%s");
                                            }
                                            args.push(val);
                                        }
                                    }
                                }
                                fmt_string.push('\n');

                                let fmt_ptr = self
                                    .builder
                                    .build_global_string_ptr(&fmt_string, "fmt_newline")
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    vec![fmt_ptr.as_pointer_value().into()];
                                for arg in args {
                                    call_args.push(arg.into());
                                }

                                self.builder
                                    .build_call(printf_fn, &call_args, "call_printf")
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                            }
                            Expression::StringLiteral(text, _) => {
                                let puts_fn = self.module.get_function("puts").unwrap();
                                let global_str = self
                                    .builder
                                    .build_global_string_ptr(text, "str_lit")
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                                self.builder
                                    .build_call(
                                        puts_fn,
                                        &[global_str.as_pointer_value().into()],
                                        "call_puts",
                                    )
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                            }
                            other => {
                                let val = self.compile_expression(other, local_vars)?;
                                let fmt_str = if val.is_int_value() { "%ld\n" } else { "%s\n" };
                                let fmt_ptr = self
                                    .builder
                                    .build_global_string_ptr(fmt_str, "fmt_single")
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                                self.builder
                                    .build_call(
                                        printf_fn,
                                        &[fmt_ptr.as_pointer_value().into(), val.into()],
                                        "call_printf",
                                    )
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                            }
                        }
                    }
                }
                Ok(self.context.i32_type().const_int(0, false).into())
            }
        }
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
