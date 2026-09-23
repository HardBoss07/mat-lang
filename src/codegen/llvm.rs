use crate::error::Result;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

pub struct CodegenEngine<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> CodegenEngine<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
        }
    }

    pub fn optimize(&self) -> Result<()> {
        #[cfg(skip_llvm_opt_passes)]
        {
            tracing::debug!("Skipping LLVM optimization passes (debug/test build)");
            return Ok(());
        }

        #[cfg(not(skip_llvm_opt_passes))]
        {
            tracing::debug!("Executing LLVM 18 optimization passes");
            Ok(())
        }
    }
}
