/// Mangles module path into global target symbols (e.g. `_mat_math_calc_add`)
pub fn mangle_symbol(module_path: &[&str], symbol_name: &str) -> String {
    if module_path.is_empty() {
        format!("_mat_{}", symbol_name)
    } else {
        format!("_mat_{}_{}", module_path.join("_"), symbol_name)
    }
}
