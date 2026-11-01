use crate::emit_ir;

#[test]
fn test_codegen_if_else_blocks_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let x: int = 5;
    if x > 3 { let a: int = 1; } else { let b: int = 2; }
}
"#,
    );
    assert!(ir.contains("if_then"));
    assert!(ir.contains("if_else"));
    assert!(ir.contains("if_after"));
    insta::assert_snapshot!("if_else_blocks_ir", ir);
}

#[test]
fn test_codegen_while_loop_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let mut i: int = 0;
    while i < 5 { i++; }
}
"#,
    );
    assert!(ir.contains("while_cond"));
    assert!(ir.contains("while_body"));
    assert!(ir.contains("while_after"));
    insta::assert_snapshot!("while_loop_ir", ir);
}

#[test]
fn test_codegen_loop_with_break_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let mut k: int = 0;
    loop {
        k++;
        if k > 10 { break; }
    }
}
"#,
    );
    assert!(ir.contains("loop_body"));
    assert!(ir.contains("loop_after"));
    insta::assert_snapshot!("loop_with_break_ir", ir);
}

#[test]
fn test_codegen_fori_loop_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    fori (let i: int = 0; i < 5; i++) { let x: int = i; }
}
"#,
    );
    assert!(ir.contains("for_cond"));
    assert!(ir.contains("for_body"));
    assert!(ir.contains("for_step"));
    assert!(ir.contains("for_after"));
    insta::assert_snapshot!("fori_loop_ir", ir);
}

#[test]
fn test_codegen_for_in_loop_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let nums: [int; 3] = [10, 20, 30];
    for n in nums { let v: int = n; }
}
"#,
    );
    assert!(ir.contains("for_in_cond"));
    assert!(ir.contains("for_in_body"));
    insta::assert_snapshot!("for_in_loop_ir", ir);
}

#[test]
fn test_codegen_nested_loop_break_continue_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let mut i: int = 0;
    while i < 10 {
        i++;
        if i == 5 { continue; }
        loop {
            if i > 8 { break; }
            i++;
        }
    }
}
"#,
    );
    assert!(ir.contains("while_cond"));
    assert!(ir.contains("loop_body"));
    insta::assert_snapshot!("nested_loop_break_continue_ir", ir);
}
