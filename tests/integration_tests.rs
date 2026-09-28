// tests/integration_tests.rs
//
// End-to-end compilation and execution tests.
// Each test compiles a .mat program to a native binary inside a tempdir,
// runs it, and asserts the exact stdout output.

mod common;
use common::compile_and_run;

// ---------------------------------------------------------------------------
// Helper: assert exact multi-line output with trailing whitespace trimmed.
// ---------------------------------------------------------------------------
fn assert_output(src: &str, expected: &str) {
    let output = compile_and_run(src);
    let got = output.trim_end_matches(['\n', '\r']);
    let want = expected.trim_end_matches(['\n', '\r']);
    assert_eq!(
        got, want,
        "\nExpected output:\n{}\n\nActual output:\n{}",
        want, got
    );
}

// ---------------------------------------------------------------------------
// 1. Hello world and plain println
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_hello_world() {
    assert_output(r#"fn main() { println("Hello, mat!"); }"#, "Hello, mat!");
}

// ---------------------------------------------------------------------------
// 2. String interpolation and format specifiers
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_string_interpolation() {
    let src = r#"
fn main() {
    let name: String = "world";
    let num: int = 42;
    println("Hello, {name}!");
    println("Number: {num}");
    let mask: int = 8;
    println("Binary: {mask:bin}");
    println("Hex: {mask:hex}");
}
"#;
    assert_output(src, "Hello, world!\nNumber: 42\nBinary: 0b1000\nHex: 0x8");
}

// ---------------------------------------------------------------------------
// 3. Escape sequences in strings
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_escape_sequences() {
    // Newline and tab escapes are encoded at parse time and printed literally.
    let src = "fn main() {\n    println(\"Line1\\nLine2\");\n    println(\"Tab:\\tEnd\");\n}";
    let output = compile_and_run(src);
    assert!(output.contains("Line1\nLine2"), "expected embedded newline");
    assert!(output.contains("Tab:\tEnd"), "expected embedded tab");
}

// ---------------------------------------------------------------------------
// 4. Arithmetic and compound mutation operators
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_math_operations() {
    let src = r#"
fn main() {
    let mut x: int = 10;
    x += 5;
    x -= 2;
    x *= 3;
    x /= 3;
    x %= 7;
    x++;
    x--;
    println(x);
}
"#;
    // 10 +5=15 -2=13 *3=39 /3=13 %7=6 ++=7 --=6
    assert_output(src, "6");
}

// ---------------------------------------------------------------------------
// 5. Bitwise shift operators
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_bitwise_shifts() {
    let src = r#"
fn main() {
    let mut m: int = 1;
    m <<= 3;
    println(m);
    m >>= 1;
    println(m);
}
"#;
    assert_output(src, "8\n4");
}

// ---------------------------------------------------------------------------
// 6. if / else if / else chains
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_if_else_chain() {
    let src = r#"
fn classify(n: int) -> String {
    if n < 0 {
        return "negative";
    } else if n == 0 {
        return "zero";
    } else {
        return "positive";
    }
}
fn main() {
    println(classify(0 - 5));
    println(classify(0));
    println(classify(7));
}
"#;
    assert_output(src, "negative\nzero\npositive");
}

// ---------------------------------------------------------------------------
// 7. while loop with continue
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_while_loop_with_continue() {
    let src = r#"
fn main() {
    let mut i: int = 0;
    let mut sum: int = 0;
    while i < 6 {
        i++;
        if i == 3 {
            continue;
        }
        sum += i;
    }
    println(sum);
}
"#;
    // i visits 1,2,3(skip),4,5,6 => sum = 1+2+4+5+6 = 18
    assert_output(src, "18");
}

// ---------------------------------------------------------------------------
// 8. fori loop with break
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_fori_with_break() {
    let src = r#"
fn main() {
    let mut total: int = 0;
    fori (let i: int = 0; i < 100; i++) {
        if i == 5 {
            break;
        }
        total += i;
    }
    println(total);
}
"#;
    // 0+1+2+3+4 = 10
    assert_output(src, "10");
}

// ---------------------------------------------------------------------------
// 9. for-in loop over int array
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_for_in_array() {
    let src = r#"
fn main() {
    let nums: [int; 4] = [10, 20, 30, 40];
    let mut sum: int = 0;
    for n in nums {
        sum += n;
    }
    println(sum);
}
"#;
    assert_output(src, "100");
}

// ---------------------------------------------------------------------------
// 10. Infinite loop with break
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_infinite_loop_break() {
    let src = r#"
fn main() {
    let mut k: int = 0;
    loop {
        k++;
        if k >= 5 {
            break;
        }
    }
    println(k);
}
"#;
    assert_output(src, "5");
}

// ---------------------------------------------------------------------------
// 11. Recursive function: factorial
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_recursive_factorial() {
    let src = r#"
fn factorial(n: int) -> int {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}
fn main() {
    println(factorial(1));
    println(factorial(5));
    println(factorial(10));
}
"#;
    assert_output(src, "1\n120\n3628800");
}

// ---------------------------------------------------------------------------
// 12. Recursive function: fibonacci
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_recursive_fibonacci() {
    let src = r#"
fn fib(n: int) -> int {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}
fn main() {
    println(fib(0));
    println(fib(1));
    println(fib(7));
    println(fib(10));
}
"#;
    assert_output(src, "0\n1\n13\n55");
}

// ---------------------------------------------------------------------------
// 13. Tuple access
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_tuple_access() {
    let src = r#"
fn main() {
    let t: (int, bool, f64) = (99, tru, 3.14);
    println(t.0);
    println(t.1);
}
"#;
    let output = compile_and_run(src);
    assert!(output.contains("99"), "expected tuple.0 = 99");
    assert!(output.contains("tru"), "expected tuple.1 = tru");
}

// ---------------------------------------------------------------------------
// 14. Array indexing and mutation
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_array_index() {
    let src = r#"
fn main() {
    let arr: [int; 5] = [10, 20, 30, 40, 50];
    println(arr[0]);
    println(arr[4]);
    let mut sum: int = 0;
    fori (let i: int = 0; i < 5; i++) {
        sum += arr[i];
    }
    println(sum);
}
"#;
    assert_output(src, "10\n50\n150");
}

// ---------------------------------------------------------------------------
// 15. Result<T, E> with match dispatch
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_result_and_match() {
    let src = r#"
fn safe_div(a: int, b: int) -> Result<int, String> {
    if b == 0 {
        return Err("division by zero");
    }
    return Ok(a / b);
}
fn main() {
    let r1: Result<int, String> = safe_div(20, 4);
    match r1 {
        Ok(v)  => println(v);
        Err(e) => println(e);
    }
    let r2: Result<int, String> = safe_div(5, 0);
    match r2 {
        Ok(v)  => println(v);
        Err(e) => println(e);
    }
}
"#;
    assert_output(src, "5\ndivision by zero");
}

// ---------------------------------------------------------------------------
// 16. Multi-function program with shared helper
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_multi_function() {
    let src = r#"
fn double(n: int) -> int {
    return n * 2;
}
fn square(n: int) -> int {
    return n * n;
}
fn main() {
    println(double(6));
    println(square(7));
    println(double(square(3)));
}
"#;
    assert_output(src, "12\n49\n18");
}

// ---------------------------------------------------------------------------
// 17. Complex boolean expression chains
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_complex_boolean_logic() {
    let src = r#"
fn main() {
    let a: int = 10;
    let b: int = 20;
    let flag: bool = tru;
    if (a < b) && flag {
        println("yes");
    } else {
        println("no");
    }
    if (a > b) || (a == 10 && flag) {
        println("maybe");
    }
}
"#;
    assert_output(src, "yes\nmaybe");
}

// ---------------------------------------------------------------------------
// 18. Nested loops
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_nested_loops() {
    let src = r#"
fn main() {
    let mut total: int = 0;
    fori (let i: int = 1; i <= 3; i++) {
        fori (let j: int = 1; j <= 3; j++) {
            total += i * j;
        }
    }
    println(total);
}
"#;
    // Each cell i*j: sum over 1<=i,j<=3 = (1+2+3)^2 = 36
    assert_output(src, "36");
}

// ---------------------------------------------------------------------------
// 19. Boolean literal output via println
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_boolean_output() {
    let src = r#"
fn main() {
    let t: bool = tru;
    let f: bool = fal;
    println(t);
    println(f);
}
"#;
    assert_output(src, "tru\nfal");
}

// ---------------------------------------------------------------------------
// 20. Float arithmetic and output
// ---------------------------------------------------------------------------

#[test]
fn test_e2e_float_arithmetic() {
    let src = r#"
fn main() {
    let a: f64 = 10.0;
    let b: f64 = 3.0;
    let div: f64 = a / b;
    let mul: f64 = a * 2.0;
    println(mul);
}
"#;
    let output = compile_and_run(src);
    assert!(
        output.contains("20"),
        "expected 20.0 or 20 in float multiply output, got: {}",
        output
    );
}
