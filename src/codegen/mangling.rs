pub fn mangle_symbol(symbol_name: &str) -> String {
    if symbol_name == "main" {
        "main".to_string()
    } else {
        format!("_mat_{}", symbol_name)
    }
}
