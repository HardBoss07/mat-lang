use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::BasicMetadataValueEnum;

use crate::ast::{Expression, FunctionDeclaration, Item, Program, Statement};
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

        for stmt in &func.body {
            match stmt {
                Statement::Expression(expr) => self.compile_expression(expr)?,
            }
        }

        self.builder
            .build_return(Some(&i32_type.const_int(0, false)))
            .map_err(|e| MatcError::CodegenError(e.to_string()))?;

        Ok(())
    }

    fn compile_expression(&self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Call {
                callee, arguments, ..
            } => {
                if callee == "println" {
                    let puts_fn = self.module.get_function("puts").unwrap();
                    if let Some(Expression::StringLiteral(text, _)) = arguments.first() {
                        let global_str = self
                            .builder
                            .build_global_string_ptr(text, "str_lit")
                            .map_err(|e| MatcError::CodegenError(e.to_string()))?;

                        let args: Vec<BasicMetadataValueEnum> =
                            vec![global_str.as_pointer_value().into()];
                        self.builder
                            .build_call(puts_fn, &args, "call_puts")
                            .map_err(|e| MatcError::CodegenError(e.to_string()))?;
                    }
                }
            }
            Expression::StringLiteral(_, _) => {}
        }
        Ok(())
    }

    pub fn emit_llvm_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }
}
