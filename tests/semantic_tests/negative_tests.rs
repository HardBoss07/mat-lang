use crate::common::check;
use matc::error::MatcError;

fn assert_type_err_contains(src: &str, substring: &str) {
    match check(src) {
        Err(MatcError::TypeError { message, .. }) => {
            assert!(
                message.contains(substring),
                "expected {:?}, got:\n  {}",
                substring,
                message
            );
        }
        Err(MatcError::UndefinedVariable { name, .. }) => {
            let msg = format!("Undefined variable: {}", name);
            assert!(msg.contains(substring) || name.contains(substring));
        }
        Err(MatcError::UndefinedFunction { name, .. }) => {
            let msg = format!("Undefined function: {}", name);
            assert!(msg.contains(substring) || name.contains(substring));
        }
        Err(other) => panic!("expected semantic error, got: {:?}", other),
        Ok(()) => panic!("expected semantic error but program passed type-checking"),
    }
}

#[test]
fn test_semantic_error_undefined_variable() {
    assert_type_err_contains("fn main() { let x: int = undefined_var; }", "Undefined");
}

#[test]
fn test_semantic_error_out_of_scope_access() {
    assert_type_err_contains(
        "fn main() { if tru { let inner_x: int = 42; } let y: int = inner_x; }",
        "Undefined",
    );
}

#[test]
fn test_semantic_error_type_mismatch_assignment() {
    assert_type_err_contains("fn main() { let x: int = \"not_an_int\"; }", "mismatch");
}

#[test]
fn test_semantic_error_non_bool_condition_if() {
    assert_type_err_contains(
        "fn main() { let x: int = 5; if x { let y: int = 1; } }",
        "bool",
    );
}

#[test]
fn test_semantic_error_non_bool_condition_while() {
    assert_type_err_contains("fn main() { let mut n: int = 5; while n { n--; } }", "bool");
}

#[test]
fn test_semantic_error_non_bool_condition_fori() {
    assert_type_err_contains(
        "fn main() { fori (let i: int = 0; i; i++) { let x: int = 1; } }",
        "bool",
    );
}

#[test]
fn test_semantic_error_tuple_index_out_of_bounds() {
    assert_type_err_contains(
        "fn main() { let t: (int, bool) = (1, tru); let x: bool = t.5; }",
        "out of bounds",
    );
}

#[test]
fn test_semantic_error_array_index_non_int() {
    assert_type_err_contains(
        "fn main() { let arr: [int; 3] = [1, 2, 3]; let elem: int = arr[tru]; }",
        "integer",
    );
}

#[test]
fn test_semantic_error_binary_op_type_mismatch() {
    assert_type_err_contains(
        "fn main() { let a: int = 1; let b: bool = tru; let c = a + b; }",
        "mismatch",
    );
}

#[test]
fn test_semantic_error_wrong_argument_count() {
    assert_type_err_contains(
        "fn add(a: int, b: int) -> int { return a + b; } fn main() { let r: int = add(1); }",
        "expects",
    );
}

#[test]
fn test_semantic_error_wrong_argument_type() {
    assert_type_err_contains(
        "fn negate(flag: bool) -> bool { return flag; } fn main() { let r: bool = negate(42); }",
        "Cannot check integer literal against type",
    );
}

#[test]
fn test_semantic_error_for_in_non_array() {
    assert_type_err_contains(
        "fn main() { let x: int = 5; for item in x { let v: int = item; } }",
        "collection",
    );
}

#[test]
fn test_semantic_error_array_length_mismatch() {
    assert_type_err_contains("fn main() { let arr: [int; 3] = [1, 2]; }", "length");
}

#[test]
fn test_semantic_error_tuple_length_mismatch() {
    assert_type_err_contains(
        "fn main() { let t: (int, bool, f64) = (1, tru); }",
        "length",
    );
}

#[test]
fn test_semantic_error_i8_overflow() {
    assert_type_err_contains("fn main() { let x: i8 = 200; }", "i8");
}

#[test]
fn test_semantic_error_return_type_mismatch() {
    assert_type_err_contains(
        "fn get_num() -> int { return tru; } fn main() { let n: int = get_num(); }",
        "mismatch",
    );
}

#[test]
fn test_semantic_error_undefined_struct() {
    assert_type_err_contains(
        "fn main() { let p = NonExistentStruct { x: 1 }; }",
        "Struct",
    );
}

#[test]
fn test_semantic_error_missing_struct_field() {
    assert_type_err_contains(
        "struct Point { x: int, y: int } fn main() { let p: Point = Point { x: 1 }; }",
        "field",
    );
}

#[test]
fn test_semantic_error_struct_field_type_mismatch() {
    assert_type_err_contains(
        "struct Point { x: int, y: int } fn main() { let p: Point = Point { x: \"not_an_int\", y: 2 }; }",
        "mismatch",
    );
}

#[test]
fn test_semantic_error_3d_array_type_mismatch() {
    assert_type_err_contains(
        "fn main() { let cube: [[[int; 2]; 2]; 2] = [[ [1, 2], [3, \"not_an_int\"] ], [[5, 6], [7, 8]]]; }",
        "mismatch",
    );
}

#[test]
fn test_semantic_error_3d_array_dimension_mismatch() {
    assert_type_err_contains(
        "fn main() { let cube: [[[int; 2]; 2]; 2] = [[ [1, 2, 3], [4, 5] ]]; }",
        "length",
    );
}

#[test]
fn test_semantic_error_enum_variant_not_found() {
    assert_type_err_contains(
        "enum Status { Active, Inactive } fn main() { let s = Status::NonExistentVariant; }",
        "variant",
    );
}

#[test]
fn test_semantic_error_enum_match_payload_type_mismatch() {
    assert_type_err_contains(
        "enum Value { Number(int) } fn main() { let v = Value::Number(10); match v { Value::Number(flag) => { let b: bool = flag; } } }",
        "mismatch",
    );
}

#[test]
fn test_semantic_error_private_field_access() {
    assert_type_err_contains(
        "import secret_module; fn main() { let s: secret_module::Secret = secret_module::Secret { private_val: 42 }; let v: int = s.private_val; }",
        "Undefined",
    );
}

#[test]
fn test_semantic_error_early_return_type_mismatch() {
    assert_type_err_contains(
        "fn compute(n: int) -> int { if n > 10 { return \"invalid_type\"; } return n; } fn main() { let res: int = compute(15); }",
        "mismatch",
    );
}

#[test]
fn test_semantic_error_variable_leakage_across_scopes() {
    assert_type_err_contains(
        "fn main() { if tru { let scoped_var: int = 100; } let leak: int = scoped_var; }",
        "Undefined",
    );
}
