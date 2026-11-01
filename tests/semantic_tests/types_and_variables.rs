use crate::common::check;

fn assert_ok(src: &str) {
    check(src).unwrap_or_else(|e| panic!("expected Ok but got error: {e}"));
}

#[test]
fn test_semantic_valid_primitives_and_inference() {
    assert_ok(
        r#"
fn main() {
    let a = 10;
    let b: int = 20;
    let c: i32 = 100;
    let d: i8 = 127;
    let e: f64 = 3.14;
    let f: f32 = 1.0;
    let g: bool = tru;
    let h: String = "hi";
    let t: (int, bool) = (1, tru);
    let arr: [int; 3] = [1, 2, 3];
}
"#,
    );
}

#[test]
fn test_semantic_valid_variable_shadowing_in_nested_scopes() {
    assert_ok(
        r#"
fn main() {
    let x: int = 1;
    if tru {
        let x: bool = fal;
        let y: bool = x;
    }
    let z: int = x;
}
"#,
    );
}

#[test]
fn test_semantic_valid_complex_boolean_expressions() {
    assert_ok(
        r#"
fn main() {
    let a: int = 10;
    let b: int = 20;
    let flag: bool = (a < b) && (b > 0);
    if flag || (a == b) {
        let x: int = 1;
    }
}
"#,
    );
}

#[test]
fn test_semantic_valid_deep_scope_shadowing() {
    assert_ok(
        r#"
fn main() {
    let x: int = 1;
    if tru {
        let x: String = "level_2";
        if tru {
            let x: bool = tru;
            let b: bool = x;
        }
        let s: String = x;
    }
    let i: int = x;
}
"#,
    );
}
