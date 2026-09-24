use inkwell::context::Context;
use inkwell::module::Module;

pub fn declare_runtime_symbols<'a>(context: &'a Context, module: &Module<'a>) {
    let i32_type = context.i32_type();
    let ptr_type = context.ptr_type(inkwell::AddressSpace::default());

    let puts_type = i32_type.fn_type(&[ptr_type.into()], false);
    module.add_function("puts", puts_type, None);

    let printf_type = i32_type.fn_type(&[ptr_type.into()], true);
    module.add_function("printf", printf_type, None);
}
