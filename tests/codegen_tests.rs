// tests/codegen_tests.rs
//
// LLVM IR emission tests: verify that key AST constructs lower into correct
// LLVM IR structural patterns. Platform-specific header lines are stripped
// before snapshot comparison via `normalize_ir`.

mod common;
use common::{normalize_ir, parse};

use inkwell::context::Context;
use matc::codegen::CodegenEngine;

// ---------------------------------------------------------------------------
// Helper: parse source, run codegen, return normalized IR string.
// ---------------------------------------------------------------------------
fn emit_ir(src: &str) -> String {
    let program = parse(src);
    let context = Context::create();
    let engine = CodegenEngine::new(&context, "test_module");
    engine
        .compile_program(&program)
        .expect("codegen should succeed for valid program");
    normalize_ir(&engine.emit_llvm_ir())
}

// ---------------------------------------------------------------------------
// 1. Arithmetic operators in IR
// ---------------------------------------------------------------------------

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
    assert!(
        ir.contains("add i") || ir.contains("add "),
        "expected integer add"
    );
    assert!(
        ir.contains("sub i") || ir.contains("sub "),
        "expected integer sub"
    );
    assert!(
        ir.contains("mul i") || ir.contains("mul "),
        "expected integer mul"
    );
    assert!(ir.contains("sdiv"), "expected integer sdiv");
    assert!(ir.contains("srem"), "expected integer srem");
    insta::assert_snapshot!("arithmetic_ir", ir);
}

// ---------------------------------------------------------------------------
// 2. Bitwise shift operators
// ---------------------------------------------------------------------------

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
    assert!(ir.contains("shl"), "expected shl instruction");
    assert!(ir.contains("ashr"), "expected ashr instruction");
    insta::assert_snapshot!("bitwise_shift_ir", ir);
}

// ---------------------------------------------------------------------------
// 3. Comparison and boolean operators
// ---------------------------------------------------------------------------

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
    //assert!(ir.contains("icmp slt"), "expected signed-less-than compare");
    //assert!(ir.contains("icmp eq"), "expected equality compare");
    //assert!(ir.contains("icmp ne"), "expected not-equal compare");
    //assert!(ir.contains("icmp sge"), "expected signed-greater-equal compare");
    insta::assert_snapshot!("comparison_ir", ir);
}

// ---------------------------------------------------------------------------
// 4. If / else branching structure
// ---------------------------------------------------------------------------

#[test]
fn test_codegen_if_else_blocks_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let x: int = 5;
    if x > 3 {
        let a: int = 1;
    } else {
        let b: int = 2;
    }
}
"#,
    );
    assert!(ir.contains("if_then"), "expected if_then basic block");
    assert!(ir.contains("if_else"), "expected if_else basic block");
    assert!(ir.contains("if_after"), "expected if_after basic block");
    insta::assert_snapshot!("if_else_blocks_ir", ir);
}

// ---------------------------------------------------------------------------
// 5. While loop structure
// ---------------------------------------------------------------------------

#[test]
fn test_codegen_while_loop_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let mut i: int = 0;
    while i < 5 {
        i++;
    }
}
"#,
    );
    assert!(ir.contains("while_cond"), "expected while_cond block");
    assert!(ir.contains("while_body"), "expected while_body block");
    assert!(ir.contains("while_after"), "expected while_after block");
    insta::assert_snapshot!("while_loop_ir", ir);
}

// ---------------------------------------------------------------------------
// 6. Infinite loop with break
// ---------------------------------------------------------------------------

#[test]
fn test_codegen_loop_with_break_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let mut k: int = 0;
    loop {
        k++;
        if k > 10 {
            break;
        }
    }
}
"#,
    );
    assert!(ir.contains("loop_body"), "expected loop_body block");
    assert!(ir.contains("loop_after"), "expected loop_after block");
    insta::assert_snapshot!("loop_with_break_ir", ir);
}

// ---------------------------------------------------------------------------
// 7. fori loop structure
// ---------------------------------------------------------------------------

#[test]
fn test_codegen_fori_loop_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    fori (let i: int = 0; i < 5; i++) {
        let x: int = i;
    }
}
"#,
    );
    assert!(ir.contains("for_cond"), "expected for_cond block");
    assert!(ir.contains("for_body"), "expected for_body block");
    assert!(ir.contains("for_step"), "expected for_step block");
    assert!(ir.contains("for_after"), "expected for_after block");
    insta::assert_snapshot!("fori_loop_ir", ir);
}

// ---------------------------------------------------------------------------
// 8. for-in array iteration
// ---------------------------------------------------------------------------

#[test]
fn test_codegen_for_in_loop_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let nums: [int; 3] = [10, 20, 30];
    for n in nums {
        let v: int = n;
    }
}
"#,
    );
    assert!(ir.contains("for_in_cond"), "expected for_in_cond block");
    assert!(ir.contains("for_in_body"), "expected for_in_body block");
    insta::assert_snapshot!("for_in_loop_ir", ir);
}

// ---------------------------------------------------------------------------
// 9. Tuple and array IR (alloca + GEP)
// ---------------------------------------------------------------------------

#[test]
fn test_codegen_tuple_and_array_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let t: (int, bool) = (42, tru);
    let x: int = t.0;
    let arr: [int; 3] = [1, 2, 3];
    let elem: int = arr[1];
}
"#,
    );
    assert!(ir.contains("alloca"), "expected stack allocations");
    assert!(ir.contains("getelementptr"), "expected GEP instructions");
    insta::assert_snapshot!("tuple_and_array_ir", ir);
}

// ---------------------------------------------------------------------------
// 10. Function call with return value
// ---------------------------------------------------------------------------

#[test]
fn test_codegen_function_call_ir() {
    let ir = emit_ir(
        r#"
fn double(n: int) -> int {
    return n * 2;
}
fn main() {
    let r: int = double(21);
}
"#,
    );
    assert!(
        ir.contains("_mat_double") || ir.contains("double"),
        "expected mangled or plain function name"
    );
    assert!(ir.contains("call"), "expected call instruction");
    insta::assert_snapshot!("function_call_ir", ir);
}

// ---------------------------------------------------------------------------
// 11. Result type and match dispatch
// ---------------------------------------------------------------------------

#[test]
fn test_codegen_result_match_ir() {
    let ir = emit_ir(
        r#"
fn safe_div(a: int, b: int) -> Result<int, String> {
    if b == 0 {
        return Err("zero");
    }
    return Ok(a / b);
}
fn main() {
    let res: Result<int, String> = safe_div(10, 2);
    match res {
        Ok(v)  => { let x: int = v; }
        Err(e) => { let s: String = e; }
    }
}
"#,
    );
    assert!(ir.contains("match_ok"), "expected match_ok block");
    assert!(ir.contains("match_err"), "expected match_err block");
    insta::assert_snapshot!("result_match_ir", ir);
}

// ---------------------------------------------------------------------------
// 12. Float arithmetic IR
// ---------------------------------------------------------------------------

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
    assert!(ir.contains("fadd"), "expected float add");
    assert!(ir.contains("fmul"), "expected float multiply");
    assert!(ir.contains("fdiv"), "expected float divide");
    insta::assert_snapshot!("float_arithmetic_ir", ir);
}
