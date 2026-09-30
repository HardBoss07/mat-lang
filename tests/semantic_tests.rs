// tests/semantic_tests.rs
//
// Semantic analysis / type checker integration tests.
// Positive tests verify valid programs pass; negative tests verify that
// specific error conditions produce semantic error variants.

mod common;
use common::check;
use matc::error::MatcError;

// ---------------------------------------------------------------------------
// Helper: assert the program type-checks successfully.
// ---------------------------------------------------------------------------
fn assert_ok(src: &str) {
    check(src).unwrap_or_else(|e| panic!("expected Ok but got error: {e}"));
}

// ---------------------------------------------------------------------------
// Helper: assert the program fails type-checking and the error message
// contains `substring`.
// ---------------------------------------------------------------------------
fn assert_type_err_contains(src: &str, substring: &str) {
    match check(src) {
        Err(MatcError::TypeError { message, .. }) => {
            assert!(
                message.contains(substring),
                "expected error message to contain {:?}, got:\n  {}",
                substring,
                message
            );
        }
        Err(MatcError::UndefinedVariable { name, .. }) => {
            let msg = format!("Undefined variable: {}", name);
            assert!(
                msg.contains(substring) || name.contains(substring),
                "expected error message to contain {:?}, got:\n  {}",
                substring,
                msg
            );
        }
        Err(MatcError::UndefinedFunction { name, .. }) => {
            let msg = format!("Undefined function: {}", name);
            assert!(
                msg.contains(substring) || name.contains(substring),
                "expected error message to contain {:?}, got:\n  {}",
                substring,
                msg
            );
        }
        Err(other) => panic!("expected semantic error, got: {:?}", other),
        Ok(()) => panic!("expected semantic error but program passed type-checking"),
    }
}

// ===========================================================================
// POSITIVE TESTS: valid programs must pass type checking
// ===========================================================================

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
    // A variable `x` may be re-declared in an inner scope (if-branch) with a
    // different type; the outer scope retains the original type.
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
fn test_semantic_valid_function_calls_and_return_types() {
    assert_ok(
        r#"
fn add(a: int, b: int) -> int {
    return a + b;
}
fn is_even(n: int) -> bool {
    if n % 2 == 0 {
        return tru;
    }
    return fal;
}
fn main() {
    let sum: int = add(3, 4);
    let flag: bool = is_even(sum);
}
"#,
    );
}

#[test]
fn test_semantic_valid_recursive_function() {
    assert_ok(
        r#"
fn fib(n: int) -> int {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}
fn main() {
    let result: int = fib(10);
}
"#,
    );
}

#[test]
fn test_semantic_valid_while_and_fori_loops() {
    assert_ok(
        r#"
fn main() {
    let mut i: int = 0;
    while i < 5 {
        i++;
    }
    fori (let j: int = 0; j < 10; j++) {
        let val: int = j;
    }
}
"#,
    );
}

#[test]
fn test_semantic_valid_for_in_array() {
    assert_ok(
        r#"
fn main() {
    let nums: [int; 4] = [1, 2, 3, 4];
    for n in nums {
        let v: int = n;
    }
    let flags: [bool; 2] = [tru, fal];
    for f in flags {
        let b: bool = f;
    }
}
"#,
    );
}

#[test]
fn test_semantic_valid_result_match() {
    assert_ok(
        r#"
fn divide(a: int, b: int) -> Result<int, String> {
    if b == 0 {
        return Err("division by zero");
    }
    return Ok(a / b);
}
fn main() {
    let res: Result<int, String> = divide(10, 2);
    match res {
        Ok(v)  => { let x: int = v; }
        Err(e) => { let s: String = e; }
    }
}
"#,
    );
}

#[test]
fn test_semantic_valid_tuple_access_and_array_index() {
    assert_ok(
        r#"
fn main() {
    let t: (int, bool, f64) = (5, tru, 3.14);
    let x: int = t.0;
    let b: bool = t.1;
    let f: f64 = t.2;
    let arr: [int; 3] = [10, 20, 30];
    let elem: int = arr[0];
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

// ===========================================================================
// NEGATIVE TESTS: type errors must be diagnosed gracefully
// ===========================================================================

#[test]
fn test_semantic_error_undefined_variable() {
    assert_type_err_contains(
        r#"
fn main() {
    let x: int = undefined_var;
}
"#,
        "Undefined",
    );
}

#[test]
fn test_semantic_error_out_of_scope_access() {
    // `inner_x` is declared inside the if-branch and must not be visible after.
    assert_type_err_contains(
        r#"
fn main() {
    if tru {
        let inner_x: int = 42;
    }
    let y: int = inner_x;
}
"#,
        "Undefined",
    );
}

#[test]
fn test_semantic_error_type_mismatch_assignment() {
    // Assigning a string literal to an `int` binding.
    assert_type_err_contains(
        r#"
fn main() {
    let x: int = "not_an_int";
}
"#,
        "mismatch",
    );
}

#[test]
fn test_semantic_error_non_bool_condition_if() {
    assert_type_err_contains(
        r#"
fn main() {
    let x: int = 5;
    if x {
        let y: int = 1;
    }
}
"#,
        "bool",
    );
}

#[test]
fn test_semantic_error_non_bool_condition_while() {
    assert_type_err_contains(
        r#"
fn main() {
    let mut n: int = 5;
    while n {
        n--;
    }
}
"#,
        "bool",
    );
}

#[test]
fn test_semantic_error_non_bool_condition_fori() {
    assert_type_err_contains(
        r#"
fn main() {
    fori (let i: int = 0; i; i++) {
        let x: int = 1;
    }
}
"#,
        "bool",
    );
}

#[test]
fn test_semantic_error_tuple_index_out_of_bounds() {
    assert_type_err_contains(
        r#"
fn main() {
    let t: (int, bool) = (1, tru);
    let x: bool = t.5;
}
"#,
        "out of bounds",
    );
}

#[test]
fn test_semantic_error_array_index_non_int() {
    assert_type_err_contains(
        r#"
fn main() {
    let arr: [int; 3] = [1, 2, 3];
    let elem: int = arr[tru];
}
"#,
        "integer",
    );
}

#[test]
fn test_semantic_error_binary_op_type_mismatch() {
    // Adding int + bool is not valid.
    assert_type_err_contains(
        r#"
fn main() {
    let a: int = 1;
    let b: bool = tru;
    let c = a + b;
}
"#,
        "mismatch",
    );
}

#[test]
fn test_semantic_error_wrong_argument_count() {
    assert_type_err_contains(
        r#"
fn add(a: int, b: int) -> int {
    return a + b;
}
fn main() {
    let r: int = add(1);
}
"#,
        "expects",
    );
}

#[test]
fn test_semantic_error_wrong_argument_type() {
    assert_type_err_contains(
        r#"
fn negate(flag: bool) -> bool {
    return flag;
}
fn main() {
    let r: bool = negate(42);
}
"#,
        "Cannot check integer literal against type",
    );
}

#[test]
fn test_semantic_error_for_in_non_array() {
    assert_type_err_contains(
        r#"
fn main() {
    let x: int = 5;
    for item in x {
        let v: int = item;
    }
}
"#,
        "array",
    );
}

#[test]
fn test_semantic_error_array_length_mismatch() {
    // Annotated as [int; 3] but 2 elements provided.
    assert_type_err_contains(
        r#"
fn main() {
    let arr: [int; 3] = [1, 2];
}
"#,
        "length",
    );
}

#[test]
fn test_semantic_error_tuple_length_mismatch() {
    assert_type_err_contains(
        r#"
fn main() {
    let t: (int, bool, f64) = (1, tru);
}
"#,
        "length",
    );
}

#[test]
fn test_semantic_error_i8_overflow() {
    // 200 does not fit in i8 (-128..=127).
    assert_type_err_contains(
        r#"
fn main() {
    let x: i8 = 200;
}
"#,
        "i8",
    );
}

#[test]
fn test_semantic_error_return_type_mismatch() {
    assert_type_err_contains(
        r#"
fn get_num() -> int {
    return tru;
}
fn main() {
    let n: int = get_num();
}
"#,
        "mismatch",
    );
}
