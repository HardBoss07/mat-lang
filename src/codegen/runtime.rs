use inkwell::context::Context;
use inkwell::module::Module;

pub fn declare_runtime_symbols<'a>(context: &'a Context, module: &Module<'a>) {
    let bool_type = context.bool_type();
    let i64_type = context.i64_type();
    let f64_type = context.f64_type();
    let void_type = context.void_type();
    let ptr_type = context.ptr_type(inkwell::AddressSpace::default());

    if module.get_function("_mat_rt_fmt_string").is_none() {
        let fmt_type = ptr_type.fn_type(&[ptr_type.into()], true);
        module.add_function("_mat_rt_fmt_string", fmt_type, None);
    }

    if module.get_function("_mat_rt_println_str").is_none() {
        let fn_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("_mat_rt_println_str", fn_type, None);
    }

    if module.get_function("_mat_rt_print_str").is_none() {
        let fn_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("_mat_rt_print_str", fn_type, None);
    }

    if module.get_function("_mat_rt_println_int").is_none() {
        let fn_type = void_type.fn_type(&[i64_type.into()], false);
        module.add_function("_mat_rt_println_int", fn_type, None);
    }

    if module.get_function("_mat_rt_println_float").is_none() {
        let fn_type = void_type.fn_type(&[f64_type.into()], false);
        module.add_function("_mat_rt_println_float", fn_type, None);
    }

    if module.get_function("_mat_rt_println_bool").is_none() {
        let fn_type = void_type.fn_type(&[bool_type.into()], false);
        module.add_function("_mat_rt_println_bool", fn_type, None);
    }
}
