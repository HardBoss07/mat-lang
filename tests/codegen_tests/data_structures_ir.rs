use crate::emit_ir;

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
    assert!(ir.contains("alloca"));
    assert!(ir.contains("getelementptr"));
    insta::assert_snapshot!("tuple_and_array_ir", ir);
}

#[test]
fn test_codegen_result_match_ir() {
    let ir = emit_ir(
        r#"
fn safe_div(a: int, b: int) -> Result<int, String> {
    if b == 0 { return Err("zero"); }
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
    assert!(ir.contains("match_ok"));
    assert!(ir.contains("match_err"));
    insta::assert_snapshot!("result_match_ir", ir);
}

#[test]
fn test_codegen_struct_init_and_field_access_ir() {
    let ir = emit_ir(
        r#"
struct Point { x: int, y: int }
fn main() {
    let p: Point = Point { x: 10, y: 20 };
    let px: int = p.x;
}
"#,
    );
    assert!(ir.contains("alloca"));
    assert!(ir.contains("getelementptr") || ir.contains("extractvalue"));
    insta::assert_snapshot!("struct_init_and_field_access_ir", ir);
}

#[test]
fn test_codegen_3d_array_access_ir() {
    let ir = emit_ir(
        r#"
fn main() {
    let cube: [[[int; 2]; 2]; 2] = [
        [[1, 2], [3, 4]],
        [[5, 6], [7, 8]]
    ];
    let val: int = cube[1][0][1];
}
"#,
    );
    assert!(ir.contains("getelementptr"));
    insta::assert_snapshot!("3d_array_access_ir", ir);
}

#[test]
fn test_codegen_enum_payload_match_ir() {
    let ir = emit_ir(
        r#"
fn safe_div(a: int, b: int) -> Result<int, String> {
    if b == 0 { return Err("zero"); }
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
    assert!(ir.contains("match_ok") || ir.contains("match_err") || ir.contains("switch"));
    insta::assert_snapshot!("enum_payload_match_ir", ir);
}
