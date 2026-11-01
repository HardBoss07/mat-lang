use crate::common::check;

fn assert_ok(src: &str) {
    check(src).unwrap_or_else(|e| panic!("expected Ok but got error: {e}"));
}

#[test]
fn test_semantic_valid_function_calls_and_return_types() {
    assert_ok(
        r#"
fn add(a: int, b: int) -> int { return a + b; }
fn is_even(n: int) -> bool {
    if n % 2 == 0 { return tru; }
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
    if n <= 1 { return n; }
    return fib(n - 1) + fib(n - 2);
}
fn main() { let result: int = fib(10); }
"#,
    );
}

#[test]
fn test_semantic_valid_while_and_fori_loops() {
    assert_ok(
        r#"
fn main() {
    let mut i: int = 0;
    while i < 5 { i++; }
    fori (let j: int = 0; j < 10; j++) { let val: int = j; }
}
"#,
    );
}

#[test]
fn test_semantic_valid_stdlib_function_calls() {
    assert_ok(
        r#"
import std::math;
fn main() {
    let x: f64 = 4.0;
    let root: f64 = sqrt(x);
}
"#,
    );
}
