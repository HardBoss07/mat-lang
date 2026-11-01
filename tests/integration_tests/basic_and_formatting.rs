use crate::assert_output;
use crate::common::compile_and_run;

#[test]
fn test_e2e_hello_world() {
    assert_output(r#"fn main() { println("Hello, mat!"); }"#, "Hello, mat!");
}

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

#[test]
fn test_e2e_escape_sequences() {
    let src = "fn main() {\n    println(\"Line1\\nLine2\");\n    println(\"Tab:\\tEnd\");\n}";
    let output = compile_and_run(src);
    assert!(output.contains("Line1\nLine2"));
    assert!(output.contains("Tab:\tEnd"));
}

#[test]
fn test_e2e_math_operations() {
    let src = r#"
fn main() {
    let mut x: int = 10;
    x += 5; x -= 2; x *= 3; x /= 3; x %= 7; x++; x--;
    println(x);
}
"#;
    assert_output(src, "6");
}

#[test]
fn test_e2e_bitwise_shifts() {
    let src = r#"
fn main() {
    let mut m: int = 1;
    m <<= 3; println(m);
    m >>= 1; println(m);
}
"#;
    assert_output(src, "8\n4");
}

#[test]
fn test_e2e_complex_boolean_logic() {
    let src = r#"
fn main() {
    let a: int = 10;
    let b: int = 20;
    let flag: bool = tru;
    if (a < b) && flag { println("yes"); } else { println("no"); }
    if (a > b) || (a == 10 && flag) { println("maybe"); }
}
"#;
    assert_output(src, "yes\nmaybe");
}

#[test]
fn test_e2e_boolean_output() {
    let src = r#"
fn main() {
    let t: bool = tru;
    let f: bool = fal;
    println(t); println(f);
}
"#;
    assert_output(src, "tru\nfal");
}

#[test]
fn test_e2e_float_arithmetic() {
    let src = r#"
fn main() {
    let a: f64 = 10.0;
    let b: f64 = 3.0;
    let mul: f64 = a * 2.0;
    println(mul);
}
"#;
    let output = compile_and_run(src);
    assert!(output.contains("20"));
}
