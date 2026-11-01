use crate::common::{compile_and_run, compile_and_run_multi};

#[test]
fn test_e2e_multi_file_module_import() {
    let files = [
        (
            "math/calc.mat",
            r#"
pub struct ResultBox { pub value: int }
pub fn add(a: int, b: int) -> int { return a + b; }
"#,
        ),
        (
            "main.mat",
            r#"
import math::calc;
fn main() {
    let sum: int = calc::add(20, 22);
    let box: calc::ResultBox = calc::ResultBox { value: sum };
    println(box.value);
}
"#,
        ),
    ];

    let output = compile_and_run_multi(&files);
    assert_eq!(output.trim(), "42");
}

#[test]
fn test_e2e_stdlib_math_integration() {
    let src = r#"
import std::math;
fn main() {
    let val: f64 = 16.0;
    let res: f64 = math::sqrt(val);
    println(res);
}
"#;
    let output = compile_and_run(src);
    assert!(output.contains("4"));
}
