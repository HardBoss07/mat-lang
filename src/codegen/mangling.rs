pub fn mangle_symbol(symbol_name: &str) -> String {
    if symbol_name == "main" || symbol_name.starts_with("_mat_rt_") {
        symbol_name.to_string()
    } else {
        format!("_mat_{}", symbol_name.replace("::", "_"))
    }
}
