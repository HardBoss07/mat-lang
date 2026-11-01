use crate::emit_ir;

#[test]
fn test_codegen_arithmetic_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let a: int = 10;
    let b: int = 3;
    let add: int = a + b;
    let sub: int = a - b;
    let mul: int = a * b;
    let div: int = a / b;
    let rem: int = a % b;
}
"#,
    );
    assert!(ir.contains("add i") || ir.contains("add "));
    assert!(ir.contains("sub i") || ir.contains("sub "));
    assert!(ir.contains("mul i") || ir.contains("mul "));
    assert!(ir.contains("sdiv"));
    assert!(ir.contains("srem"));
    insta::assert_snapshot!("arithmetic_ir", ir);
}

#[test]
fn test_codegen_bitwise_shift_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let mut mask: int = 1;
    mask <<= 3;
    mask >>= 1;
    let shifted: int = mask << 2;
}
"#,
    );
    assert!(ir.contains("shl"));
    assert!(ir.contains("ashr"));
    insta::assert_snapshot!("bitwise_shift_ir", ir);
}

#[test]
fn test_codegen_comparison_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let a: int = 5;
    let b: int = 10;
    let lt: bool = a < b;
    let eq: bool = a == b;
    let ne: bool = a != b;
    let ge: bool = b >= a;
    let and: bool = lt && eq;
    let or: bool = ne || ge;
}
"#,
    );
    insta::assert_snapshot!("comparison_ir", ir);
}

#[test]
fn test_codegen_float_arithmetic_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let a: f64 = 3.14;
    let b: f64 = 2.0;
    let add: f64 = a + b;
    let mul: f64 = a * b;
    let div: f64 = a / b;
}
"#,
    );
    assert!(ir.contains("fadd"));
    assert!(ir.contains("fmul"));
    assert!(ir.contains("fdiv"));
    insta::assert_snapshot!("float_arithmetic_ir", ir);
}

#[test]
fn test_codegen_function_call_ir() {
    let ir = emit_ir(
        r#"
fn double(n: int) -> int { return n * 2; }
fn main() { let r: int = double(21); }
"#,
    );
    assert!(ir.contains("_mat_double") || ir.contains("double"));
    assert!(ir.contains("call"));
    insta::assert_snapshot!("function_call_ir", ir);
}

#[test]
fn test_codegen_stdlib_import_ir() {
    let ir = emit_ir(
        r#"
fn sqrt(x: f64) -> f64 { return x; }
fn main() {
    let val: f64 = 9.0;
    let s: f64 = sqrt(val);
}
"#,
    );
    assert!(ir.contains("call double") || ir.contains("define double"));
    insta::assert_snapshot!("stdlib_import_ir", ir);
}
