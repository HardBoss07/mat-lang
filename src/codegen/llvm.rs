use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{AsValueRef, BasicValueEnum, PointerValue};
use std::collections::HashMap;
use std::path::Path;

use crate::ast::{Expression, FunctionDeclaration, Item, Program, Statement, Type};
use crate::codegen::mangling::mangle_symbol;
use crate::codegen::runtime::declare_runtime_symbols;
use crate::error::{MatcError, Result};
use crate::semantic::TypeChecker;

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

    fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Int => self.context.i64_type().into(),
            Type::I32 => self.context.i32_type().into(),
            Type::I16 => self.context.i16_type().into(),
            Type::I8 => self.context.i8_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::F32 => self.context.f32_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::String => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            Type::Tuple(elems) => {
                let llvm_elems: Vec<BasicTypeEnum<'ctx>> =
                    elems.iter().map(|t| self.llvm_type(t)).collect();
                self.context.struct_type(&llvm_elems, false).into()
            }
            Type::Array(elem_ty, len) => {
                let elem_llvm = self.llvm_type(elem_ty);
                elem_llvm.array_type(*len as u32).into()
            }
            _ => self.context.i64_type().into(),
        }
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

        let mut local_vars: HashMap<String, (PointerValue<'ctx>, Type)> = HashMap::new();
        let mut symbol_table = crate::semantic::SymbolTable::new();
        let type_checker = TypeChecker::new();

        for stmt in &func.body {
            match stmt {
                Statement::Let {
                    name, ty, value, ..
                } => {
                    let mat_ty = match ty {
                        Some(explicit_ty) => {
                            type_checker.check_expr(value, explicit_ty, &symbol_table)?
                        }
                        None => type_checker.synthesize_expr(value, &symbol_table)?,
                    };

                    symbol_table.insert(name.clone(), mat_ty.clone(), false);

                    let val =
                        self.compile_expression(value, &local_vars, &symbol_table, &type_checker)?;
                    let llvm_ty = self.llvm_type(&mat_ty);

                    let alloca = self
                        .builder
                        .build_alloca(llvm_ty, name)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                    self.builder
                        .build_store(alloca, val)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                    local_vars.insert(name.clone(), (alloca, mat_ty));
                }
                Statement::Assignment { target, value, .. } => {
                    let val =
                        self.compile_expression(value, &local_vars, &symbol_table, &type_checker)?;
                    let (ptr, _) = local_vars.get(target).ok_or_else(|| {
                        MatcError::CodegenError(format!(
                            "Undefined variable in codegen: {}",
                            target
                        ))
                    })?;
                    self.builder
                        .build_store(*ptr, val)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }
                Statement::Increment { target, .. } => {
                    let (ptr, ty) = local_vars.get(target).ok_or_else(|| {
                        MatcError::CodegenError(format!(
                            "Undefined variable in codegen: {}",
                            target
                        ))
                    })?;
                    let llvm_ty = self.llvm_type(ty);
                    let loaded = self
                        .builder
                        .build_load(llvm_ty, *ptr, target)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                        .into_int_value();

                    let one = loaded.get_type().const_int(1, false);
                    let inc = self
                        .builder
                        .build_int_add(loaded, one, "inc")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                    self.builder
                        .build_store(*ptr, inc)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }
                Statement::Decrement { target, .. } => {
                    let (ptr, ty) = local_vars.get(target).ok_or_else(|| {
                        MatcError::CodegenError(format!(
                            "Undefined variable in codegen: {}",
                            target
                        ))
                    })?;
                    let llvm_ty = self.llvm_type(ty);
                    let loaded = self
                        .builder
                        .build_load(llvm_ty, *ptr, target)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                        .into_int_value();

                    let one = loaded.get_type().const_int(1, false);
                    let dec = self
                        .builder
                        .build_int_sub(loaded, one, "dec")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                    self.builder
                        .build_store(*ptr, dec)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }
                Statement::Expression(expr) => {
                    self.compile_expression(expr, &local_vars, &symbol_table, &type_checker)?;
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
        symbols: &crate::semantic::SymbolTable,
        tc: &TypeChecker,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expression::IntLiteral(val, _) => {
                Ok(self.context.i64_type().const_int(*val as u64, true).into())
            }
            Expression::FloatLiteral(val, _) => {
                Ok(self.context.f64_type().const_float(*val).into())
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
                let llvm_ty = self.llvm_type(ty);
                let loaded = self
                    .builder
                    .build_load(llvm_ty, *ptr, name)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::TupleLiteral(elements, _) => {
                let mut field_values = Vec::new();
                let mut field_types = Vec::new();
                for elem in elements {
                    let val = self.compile_expression(elem, local_vars, symbols, tc)?;
                    field_types.push(val.get_type());
                    field_values.push(val);
                }

                let struct_ty = self.context.struct_type(&field_types, false);
                let alloca = self
                    .builder
                    .build_alloca(struct_ty, "tuple_tmp")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                for (idx, val) in field_values.into_iter().enumerate() {
                    let field_ptr = self
                        .builder
                        .build_struct_gep(struct_ty, alloca, idx as u32, "tuple_gep")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                    self.builder
                        .build_store(field_ptr, val)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                let loaded = self
                    .builder
                    .build_load(struct_ty, alloca, "tuple_val")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::ArrayLiteral(elements, _) => {
                if elements.is_empty() {
                    return Err(MatcError::CodegenError(
                        "Cannot codegen empty array".to_string(),
                    ));
                }

                let mut compiled_elements = Vec::new();
                for elem in elements {
                    compiled_elements.push(self.compile_expression(elem, local_vars, symbols, tc)?);
                }

                let elem_llvm_ty = compiled_elements[0].get_type();
                let array_ty = elem_llvm_ty.array_type(elements.len() as u32);
                let alloca = self
                    .builder
                    .build_alloca(array_ty, "arr_tmp")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let zero = self.context.i32_type().const_int(0, false);
                for (idx, val) in compiled_elements.into_iter().enumerate() {
                    let idx_val = self.context.i32_type().const_int(idx as u64, false);
                    let elem_ptr = unsafe {
                        self.builder
                            .build_gep(array_ty, alloca, &[zero, idx_val], "arr_gep")
                            .map_err(|e| MatcError::CodegenError(e.to_string()))?
                    };
                    self.builder
                        .build_store(elem_ptr, val)
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                }

                let loaded = self
                    .builder
                    .build_load(array_ty, alloca, "arr_val")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::TupleAccess { expr, index, .. } => {
                let tuple_val = self.compile_expression(expr, local_vars, symbols, tc)?;
                if let BasicValueEnum::StructValue(struct_val) = tuple_val {
                    let extracted = self
                        .builder
                        .build_extract_value(struct_val, *index as u32, "tuple_extract")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                    Ok(extracted)
                } else {
                    Err(MatcError::CodegenError(
                        "Expected struct value for tuple access".to_string(),
                    ))
                }
            }
            Expression::ArrayAccess { expr, index, .. } => {
                let array_mat_ty = tc.synthesize_expr(expr, symbols)?;
                let elem_mat_ty = match array_mat_ty {
                    Type::Array(ref elem, _) => *elem.clone(),
                    _ => return Err(MatcError::CodegenError("Expected array type".to_string())),
                };

                let array_llvm_ty = self.llvm_type(&array_mat_ty);
                let elem_llvm_ty = self.llvm_type(&elem_mat_ty);

                let array_val = self.compile_expression(expr, local_vars, symbols, tc)?;
                let index_val = self
                    .compile_expression(index, local_vars, symbols, tc)?
                    .into_int_value();

                let alloca = self
                    .builder
                    .build_alloca(array_llvm_ty, "arr_access_tmp")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                self.builder
                    .build_store(alloca, array_val)
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let zero = self.context.i32_type().const_int(0, false);
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(array_llvm_ty, alloca, &[zero, index_val], "arr_elem_gep")
                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                };

                let loaded = self
                    .builder
                    .build_load(elem_llvm_ty, elem_ptr, "arr_elem_val")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                Ok(loaded)
            }
            Expression::InterpolatedString(parts, _) => {
                let mut fmt_string = String::new();
                let mut args: Vec<BasicValueEnum<'ctx>> = Vec::new();

                for part in parts {
                    match part {
                        Expression::StringLiteral(s, _) => {
                            fmt_string.push_str(&s.replace('%', "%%"))
                        }
                        other => {
                            let val = self.compile_expression(other, local_vars, symbols, tc)?;
                            if val.is_int_value() {
                                let int_val = val.into_int_value();
                                if int_val.get_type().get_bit_width() == 1 {
                                    fmt_string.push_str("%s");
                                    let tru_ptr = self
                                        .builder
                                        .build_global_string_ptr("tru", "str_tru")
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                                        .as_pointer_value();
                                    let fal_ptr = self
                                        .builder
                                        .build_global_string_ptr("fal", "str_fal")
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                                        .as_pointer_value();

                                    let bool_str = self
                                        .builder
                                        .build_select(int_val, tru_ptr, fal_ptr, "bool_str")
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                                    args.push(bool_str);
                                } else {
                                    fmt_string.push_str("%lld");
                                    let i64_val = if int_val.get_type().get_bit_width() < 64 {
                                        self.builder
                                            .build_int_s_extend(
                                                int_val,
                                                self.context.i64_type(),
                                                "i64_ext",
                                            )
                                            .map_err(|e| MatcError::CodegenError(e.to_string()))?
                                            .into()
                                    } else {
                                        val
                                    };
                                    args.push(i64_val);
                                }
                            } else if val.is_float_value() {
                                fmt_string.push_str("%g");
                                args.push(val);
                            } else if val.is_pointer_value() {
                                fmt_string.push_str("%s");
                                args.push(val);
                            }
                        }
                    }
                }

                let fmt_fn = self.module.get_function("_mat_rt_fmt_string").unwrap();
                let fmt_ptr = self
                    .builder
                    .build_global_string_ptr(&fmt_string, "fmt")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                    vec![fmt_ptr.as_pointer_value().into()];
                for arg in &args {
                    call_args.push((*arg).into());
                }

                let call_fmt = self
                    .builder
                    .build_call(fmt_fn, &call_args, "call_fmt")
                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                let res_ptr = unsafe { BasicValueEnum::new(call_fmt.as_value_ref()) };
                Ok(res_ptr)
            }
            Expression::Call {
                callee, arguments, ..
            } => {
                if callee == "println" {
                    if let Some(first_arg) = arguments.first() {
                        let val = self.compile_expression(first_arg, local_vars, symbols, tc)?;
                        let println_str_fn =
                            self.module.get_function("_mat_rt_println_str").unwrap();
                        let println_int_fn =
                            self.module.get_function("_mat_rt_println_int").unwrap();
                        let println_float_fn =
                            self.module.get_function("_mat_rt_println_float").unwrap();
                        let println_bool_fn =
                            self.module.get_function("_mat_rt_println_bool").unwrap();

                        if val.is_pointer_value() {
                            self.builder
                                .build_call(println_str_fn, &[val.into()], "call_println_str")
                                .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                        } else if val.is_int_value() {
                            let int_val = val.into_int_value();
                            if int_val.get_type().get_bit_width() == 1 {
                                self.builder
                                    .build_call(
                                        println_bool_fn,
                                        &[int_val.into()],
                                        "call_println_bool",
                                    )
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                            } else {
                                let i64_val = if int_val.get_type().get_bit_width() < 64 {
                                    self.builder
                                        .build_int_s_extend(
                                            int_val,
                                            self.context.i64_type(),
                                            "i64_ext",
                                        )
                                        .map_err(|e| MatcError::CodegenError(e.to_string()))?
                                        .into()
                                } else {
                                    val
                                };
                                self.builder
                                    .build_call(
                                        println_int_fn,
                                        &[i64_val.into()],
                                        "call_println_int",
                                    )
                                    .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                            }
                        } else if val.is_float_value() {
                            self.builder
                                .build_call(println_float_fn, &[val.into()], "call_println_float")
                                .map_err(|e| MatcError::CodegenError(e.to_string()))?;
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
