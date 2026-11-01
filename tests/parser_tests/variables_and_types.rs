use crate::common::parse;

#[test]
fn test_parser_variables_and_literals_ast() {
    let src = r#"
fn main() {
    let a: int = 42;
    let b: int = 1000000;
    let c: int = 0b10100100;
    let d: int = 0xFF00;
    let e: f64 = 3.14159;
    let f: bool = tru;
    let g: bool = fal;
    let h: String = "hello";
    let i: i8 = 127;
    let t: (int, bool, f64) = (10, tru, 2.5);
    let arr: [int; 4] = [1, 2, 3, 4];
    let mut m: int = 0;
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("variables_and_literals_ast", prog);
}

#[test]
fn test_parser_string_interpolation_ast() {
    let src = "fn main() {\n    let val: int = 8;\n    let s1: String = \"plain string\";\n    let s2: String = \"value is {val}\";\n    let s3: String = \"binary: {val:bin}\";\n    let s4: String = \"hex: {val:hex}\";\n    let s5: String = \"short-b: {val:b}\";\n    let s6: String = \"short-x: {val:x}\";\n    let s7: String = \"mixed {val} text {val:bin} end\";\n}";
    let prog = parse(src);
    insta::assert_yaml_snapshot!("string_interpolation_ast", prog);
}

#[test]
fn test_parser_skips_comments() {
    let src_with_comments = r#"
/* Comment block */
fn main() {
    let x: int = 5; // single-line comment
    println("x = {x}");
}
"#;
    let src_without_comments = r#"
fn main() {
    let x: int = 5;
    println("x = {x}");
}
"#;
    let prog_with_comments = parse(src_with_comments);
    let prog_clean = parse(src_without_comments);

    let json_with_comments = serde_json::to_value(&prog_with_comments).unwrap();
    let json_clean = serde_json::to_value(&prog_clean).unwrap();

    assert_eq!(json_with_comments, json_clean);
}

#[test]
fn test_parser_comments_ast() {
    let src = r#"
/* Comment block */
fn main() {
    let x: int = 5; // single-line comment
    println("x = {x}");
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("comments_ast", prog);
}
