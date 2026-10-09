use inkwell::context::Context;
use inkwell::module::Module;

pub fn declare_runtime_symbols<'a>(context: &'a Context, module: &Module<'a>) {
    let bool_type = context.bool_type();
    let i32_type = context.i32_type();
    let i64_type = context.i64_type();
    let f64_type = context.f64_type();
    let void_type = context.void_type();
    let ptr_type = context.ptr_type(inkwell::AddressSpace::default());

    if module.get_function("_mat_rt_init").is_none() {
        let fn_type = void_type.fn_type(&[], false);
        module.add_function("_mat_rt_init", fn_type, None);
    }

    if module.get_function("_mat_rt_fmt_string").is_none() {
        let fmt_type = ptr_type.fn_type(&[ptr_type.into()], true);
        module.add_function("_mat_rt_fmt_string", fmt_type, None);
    }

    if module.get_function("_mat_rt_fmt_bin").is_none() {
        let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
        module.add_function("_mat_rt_fmt_bin", fn_type, None);
    }

    if module.get_function("_mat_rt_fmt_hex").is_none() {
        let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
        module.add_function("_mat_rt_fmt_hex", fn_type, None);
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

    if module.get_function("_mat_rt_read_line").is_none() {
        let fn_type = ptr_type.fn_type(&[], false);
        module.add_function("_mat_rt_read_line", fn_type, None);
    }

    // Dedicated Stream File I/O Runtime Helpers
    if module.get_function("_mat_rt_file_open").is_none() {
        let fn_type = ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        module.add_function("_mat_rt_file_open", fn_type, None);
    }

    if module.get_function("_mat_rt_file_close").is_none() {
        let fn_type = bool_type.fn_type(&[ptr_type.into()], false);
        module.add_function("_mat_rt_file_close", fn_type, None);
    }

    if module.get_function("_mat_rt_file_read_line").is_none() {
        let fn_type = ptr_type.fn_type(&[ptr_type.into()], false);
        module.add_function("_mat_rt_file_read_line", fn_type, None);
    }

    if module.get_function("_mat_rt_file_read_to_string").is_none() {
        let fn_type = ptr_type.fn_type(&[ptr_type.into()], false);
        module.add_function("_mat_rt_file_read_to_string", fn_type, None);
    }

    if module.get_function("_mat_rt_file_write").is_none() {
        let fn_type = i64_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        module.add_function("_mat_rt_file_write", fn_type, None);
    }

    if module.get_function("_mat_rt_file_flush").is_none() {
        let fn_type = bool_type.fn_type(&[ptr_type.into()], false);
        module.add_function("_mat_rt_file_flush", fn_type, None);
    }

    if module.get_function("_mat_rt_file_seek").is_none() {
        let fn_type = i64_type.fn_type(&[ptr_type.into(), i64_type.into(), i32_type.into()], false);
        module.add_function("_mat_rt_file_seek", fn_type, None);
    }

    // Dynamic Vector Runtime Implementation
    if module.get_function("_mat_rt_vec_create").is_none() {
        let fn_type = ptr_type.fn_type(&[i64_type.into(), i64_type.into()], false);
        module.add_function("_mat_rt_vec_create", fn_type, None);
    }

    if module.get_function("_mat_rt_vec_push").is_none() {
        let fn_type = void_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        module.add_function("_mat_rt_vec_push", fn_type, None);
    }

    if module.get_function("_mat_rt_vec_pop").is_none() {
        let fn_type = void_type.fn_type(&[ptr_type.into(), ptr_type.into()], false);
        module.add_function("_mat_rt_vec_pop", fn_type, None);
    }

    if module.get_function("_mat_rt_vec_get").is_none() {
        let fn_type = ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        module.add_function("_mat_rt_vec_get", fn_type, None);
    }

    if module.get_function("_mat_rt_vec_len").is_none() {
        let fn_type = i64_type.fn_type(&[ptr_type.into()], false);
        module.add_function("_mat_rt_vec_len", fn_type, None);
    }

    // Mathematical Primitives
    let unary_f64_fns = [
        "_mat_rt_sin",
        "_mat_rt_cos",
        "_mat_rt_tan",
        "_mat_rt_asin",
        "_mat_rt_acos",
        "_mat_rt_atan",
        "_mat_rt_sqrt",
        "_mat_rt_abs_f64",
        "_mat_rt_floor",
        "_mat_rt_ceil",
        "_mat_rt_round",
    ];

    for fn_name in unary_f64_fns {
        if module.get_function(fn_name).is_none() {
            let fn_type = f64_type.fn_type(&[f64_type.into()], false);
            module.add_function(fn_name, fn_type, None);
        }
    }

    let binary_f64_fns = ["_mat_rt_atan2", "_mat_rt_pow"];
    for fn_name in binary_f64_fns {
        if module.get_function(fn_name).is_none() {
            let fn_type = f64_type.fn_type(&[f64_type.into(), f64_type.into()], false);
            module.add_function(fn_name, fn_type, None);
        }
    }

    if module.get_function("_mat_rt_abs_int").is_none() {
        let fn_type = i64_type.fn_type(&[i64_type.into()], false);
        module.add_function("_mat_rt_abs_int", fn_type, None);
    }
}
