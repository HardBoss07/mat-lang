use crate::assert_output;

#[test]
fn test_e2e_if_else_chain() {
    let src = r#"
fn classify(n: int) -> String {
    if n < 0 { return "negative"; }
    else if n == 0 { return "zero"; }
    else { return "positive"; }
}
fn main() {
    println(classify(0 - 5));
    println(classify(0));
    println(classify(7));
}
"#;
    assert_output(src, "negative\nzero\npositive");
}

#[test]
fn test_e2e_while_loop_with_continue() {
    let src = r#"
fn main() {
    let mut i: int = 0;
    let mut sum: int = 0;
    while i < 6 {
        i++;
        if i == 3 { continue; }
        sum += i;
    }
    println(sum);
}
"#;
    assert_output(src, "18");
}

#[test]
fn test_e2e_fori_with_break() {
    let src = r#"
fn main() {
    let mut total: int = 0;
    fori (let i: int = 0; i < 100; i++) {
        if i == 5 { break; }
        total += i;
    }
    println(total);
}
"#;
    assert_output(src, "10");
}

#[test]
fn test_e2e_for_in_array() {
    let src = r#"
fn main() {
    let nums: [int; 4] = [10, 20, 30, 40];
    let mut sum: int = 0;
    for n in nums { sum += n; }
    println(sum);
}
"#;
    assert_output(src, "100");
}

#[test]
fn test_e2e_infinite_loop_break() {
    let src = r#"
fn main() {
    let mut k: int = 0;
    loop {
        k++;
        if k >= 5 { break; }
    }
    println(k);
}
"#;
    assert_output(src, "5");
}

#[test]
fn test_e2e_recursive_factorial() {
    let src = r#"
fn factorial(n: int) -> int {
    if n <= 1 { return 1; }
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

#[test]
fn test_e2e_recursive_fibonacci() {
    let src = r#"
fn fib(n: int) -> int {
    if n <= 1 { return n; }
    return fib(n - 1) + fib(n - 2);
}
fn main() {
    println(fib(0)); println(fib(1)); println(fib(7)); println(fib(10));
}
"#;
    assert_output(src, "0\n1\n13\n55");
}

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
    assert_output(src, "36");
}

#[test]
fn test_e2e_deep_scope_variable_shadowing() {
    let src = r#"
fn main() {
    let x: int = 10;
    println(x);
    if tru {
        let x: int = 20;
        println(x);
        fori (let i: int = 0; i < 1; i++) {
            let x: int = 30;
            println(x);
        }
        println(x);
    }
    println(x);
}
"#;
    assert_output(src, "10\n20\n30\n20\n10");
}

#[test]
fn test_e2e_nested_loops_with_targeted_break_and_continue() {
    let src = r#"
fn main() {
    let mut total: int = 0;
    fori (let i: int = 1; i <= 3; i++) {
        if i == 2 { continue; }
        let mut j: int = 0;
        while j < 5 {
            j++;
            if j == 3 { break; }
            total += i * 10 + j;
        }
    }
    println(total);
}
"#;
    assert_output(src, "86");
}

#[test]
fn test_e2e_early_return_in_nested_loop() {
    let src = r#"
fn find_first_pair_product(target: int) -> int {
    fori (let i: int = 1; i <= 10; i++) {
        fori (let j: int = 1; j <= 10; j++) {
            if i * j == target { return i * 100 + j; }
        }
    }
    return 0 - 1;
}
fn main() {
    println(find_first_pair_product(12));
    println(find_first_pair_product(999));
}
"#;
    assert_output(src, "206\n-1");
}
