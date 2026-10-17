// tests/parser_tests.rs
//
// Structural AST snapshot tests for the mat-lang parser.
// Each test focuses on a distinct grammar domain. Spans are stripped from
// snapshots via `#[serde(skip)]` on AST span fields.

mod common;

use common::parse;
use matc::parser::Parser;

// ---------------------------------------------------------------------------
// 1. Variables, literals, and basic types
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// 2. String interpolation and escape sequences
// ---------------------------------------------------------------------------

#[test]
fn test_parser_string_interpolation_ast() {
    let src = "fn main() {\n    let val: int = 8;\n    let s1: String = \"plain string\";\n    let s2: String = \"value is {val}\";\n    let s3: String = \"binary: {val:bin}\";\n    let s4: String = \"hex: {val:hex}\";\n    let s5: String = \"short-b: {val:b}\";\n    let s6: String = \"short-x: {val:x}\";\n    let s7: String = \"mixed {val} text {val:bin} end\";\n}";
    let prog = parse(src);
    insta::assert_yaml_snapshot!("string_interpolation_ast", prog);
}

// ---------------------------------------------------------------------------
// 3. Control flow: if / else if / else
// ---------------------------------------------------------------------------

#[test]
fn test_parser_if_else_chain_ast() {
    let src = r#"
fn main() {
    let x: int = 5;
    if x > 10 {
        let a: int = 1;
    } else if x > 3 {
        let b: int = 2;
    } else {
        let c: int = 3;
    }
    if tru {
        let d: bool = fal;
    }
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("if_else_chain_ast", prog);
}

// ---------------------------------------------------------------------------
// 4. All loop variants
// ---------------------------------------------------------------------------

#[test]
fn test_parser_all_loops_ast() {
    let src = r#"
fn main() {
    let mut i: int = 0;
    while i < 10 {
        i++;
        if i == 5 {
            continue;
        }
    }
    loop {
        i--;
        if i == 0 {
            break;
        }
    }
    fori (let j: int = 0; j < 5; j++) {
        let tmp: int = j;
    }
    let nums: [int; 3] = [10, 20, 30];
    for n in nums {
        let v: int = n;
    }
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("all_loops_ast", prog);
}

// ---------------------------------------------------------------------------
// 5. Match: Ok / Err / literal / wildcard
// ---------------------------------------------------------------------------

#[test]
fn test_parser_match_patterns_ast() {
    let src = r#"
fn check(code: int) -> Result<int, String> {
    return Ok(code);
}
fn main() {
    let res: Result<int, String> = check(200);
    match res {
        Ok(v) => {
            let ok_val: int = v;
        }
        Err(e) => {
            let err_val: String = e;
        }
    }
    let code: int = 404;
    match code {
        200 => { let a: int = 1; }
        404 => { let b: int = 2; }
        _   => { let c: int = 3; }
    }
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("match_patterns_ast", prog);
}

// ---------------------------------------------------------------------------
// 6. Functions: params, return types, recursive calls
// ---------------------------------------------------------------------------

#[test]
fn test_parser_functions_ast() {
    let src = r#"
fn no_args() {
    let x: int = 1;
}
fn one_arg(n: int) -> int {
    return n;
}
fn multi_args(a: int, b: bool, c: f64) -> f64 {
    return c;
}
fn factorial(n: int) -> int {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}
fn main() {
    no_args();
    let r: int = one_arg(42);
    let fact: int = factorial(5);
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("functions_ast", prog);
}

// ---------------------------------------------------------------------------
// 7. Operator precedence
// ---------------------------------------------------------------------------

#[test]
fn test_parser_operator_precedence_ast() {
    let src = r#"
fn main() {
    let a: int = 2 + 3 * 4;
    let b: int = 10 / 2 - 1;
    let c: int = 7 % 3;
    let d: int = 1 << 3;
    let e: int = 16 >> 2;
    let f: bool = 1 < 2;
    let g: bool = 3 >= 3;
    let h: bool = 5 == 5;
    let i: bool = 5 != 6;
    let j: bool = tru && fal;
    let k: bool = fal || tru;
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("operator_precedence_ast", prog);
}

// ---------------------------------------------------------------------------
// 8. Compound assignments and increment/decrement
// ---------------------------------------------------------------------------

#[test]
fn test_parser_mutation_operators_ast() {
    let src = r#"
fn main() {
    let mut x: int = 10;
    x += 5;
    x -= 2;
    x *= 3;
    x /= 2;
    x %= 3;
    x <<= 1;
    x >>= 2;
    x++;
    x--;
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("mutation_operators_ast", prog);
}

// ---------------------------------------------------------------------------
// 9. Tuple and array indexing access
// ---------------------------------------------------------------------------

#[test]
fn test_parser_collection_access_ast() {
    let src = r#"
fn main() {
    let t: (int, bool, f64) = (5, tru, 1.5);
    let t0: int = t.0;
    let t1: bool = t.1;
    let t2: f64 = t.2;
    let arr: [int; 3] = [10, 20, 30];
    let elem: int = arr[0];
    let elem2: int = arr[2];
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("collection_access_ast", prog);
}

// ---------------------------------------------------------------------------
// 10. Negative syntax error tests - parser must fail gracefully
// ---------------------------------------------------------------------------

#[test]
fn test_parser_error_missing_fn_parens() {
    let src = "fn main {\n    let x: int = 1;\n}\n";
    let result = Parser::new("test.mat", src).parse_program();
    assert!(
        result.is_err(),
        "expected a parse error for missing parentheses, got Ok"
    );
}

#[test]
fn test_parser_error_missing_semicolon() {
    let src = "fn main() {\n    let x: int = 1\n}\n";
    let result = Parser::new("test.mat", src).parse_program();
    assert!(
        result.is_err(),
        "expected a parse error for missing semicolon, got Ok"
    );
}

#[test]
fn test_parser_error_unclosed_block() {
    let src = "fn main() {\n    let x: int = 1;\n";
    let result = Parser::new("test.mat", src).parse_program();
    assert!(
        result.is_err(),
        "expected a parse error for unclosed block, got Ok"
    );
}

#[test]
fn test_parser_error_bad_type_annotation() {
    // `!!bool` is not a valid type
    let src = "fn main() {\n    let x: !!bool = tru;\n}\n";
    let result = Parser::new("test.mat", src).parse_program();
    assert!(
        result.is_err(),
        "expected a parse error for invalid type annotation, got Ok"
    );
}

#[test]
fn test_parser_error_bad_token_in_expression() {
    // `@` is not a valid token in mat-lang
    let src = "fn main() {\n    let x: int = @5;\n}\n";
    let result = Parser::new("test.mat", src).parse_program();
    assert!(
        result.is_err(),
        "expected a parse error for invalid token, got Ok"
    );
}

// ---------------------------------------------------------------------------
// 11. Comments: single-line and multi-line block comments
// ---------------------------------------------------------------------------

#[test]
fn test_parser_skips_comments() {
    let src_with_comments = r#"
/*
This is a comment block that can be used to describe the purpose of the code, provide author information, or any other relevant details. Comments are ignored by the compiler and are meant for developers to understand the code better.
*/
fn main() {
    let x: int = 5; // This is a single-line comment explaining that x is initialized to 5
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

    assert_eq!(
        json_with_comments, json_clean,
        "AST generated from commented code must match clean code AST (ignoring spans)"
    );
}

#[test]
fn test_parser_comments_ast() {
    let src = r#"
/*
This is a comment block that can be used to describe the purpose of the code, provide author information, or any other relevant details. Comments are ignored by the compiler and are meant for developers to understand the code better.
*/
fn main() {
    let x: int = 5; // This is a single-line comment explaining that x is initialized to 5
    println("x = {x}");
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("comments_ast", prog);
}

// ---------------------------------------------------------------------------
// 12. Struct and Field Operations
// ---------------------------------------------------------------------------

#[test]
fn test_parser_struct_declaration_and_init_ast() {
    let src = r#"
pub struct Point {
    pub x: int,
    y: int,
}

fn main() {
    let p: Point = Point { x: 10, y: 20 };
    let px: int = p.x;
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("struct_declaration_and_init_ast", prog);
}

#[test]
fn test_parser_error_missing_struct_field_colon() {
    let src = r#"
struct Point {
    x int
}
"#;
    let result = Parser::new("test.mat", src).parse_program();
    assert!(
        result.is_err(),
        "expected parse error for missing colon in struct field"
    );
}

// ---------------------------------------------------------------------------
// 13. Multi-Dimensional Arrays and Indexing
// ---------------------------------------------------------------------------

#[test]
fn test_parser_3d_array_ast() {
    let src = r#"
fn main() {
    let cube: [[[int; 2]; 2]; 2] = [
        [[1, 2], [3, 4]],
        [[5, 6], [7, 8]]
    ];
    let val: int = cube[1][0][1];
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("3d_array_ast", prog);
}

#[test]
fn test_parser_complex_enum_and_match_ast() {
    let src = r#"
pub enum Shape {
    Circle(f64),
    Rectangle(int, int),
    Label { title: String, is_visible: bool },
    Empty,
}

fn main() {
    let s: Shape = Shape::Circle(3.14);
    match s {
        Shape::Circle(r) => { let area: f64 = r; }
        Shape::Rectangle(w, h) => { let perim: int = w + h; }
        Shape::Label { title, is_visible } => { let t: String = title; }
        Shape::Empty => { let x: int = 0; }
    }
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("complex_enum_and_match_ast", prog);
}

#[test]
fn test_parser_import_declarations_ast() {
    let src = r#"
import std::io;
import std::math;
import geometry::calc;

fn main() {
    let x: int = 10;
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("import_declarations_ast", prog);
}

#[test]
fn test_parser_error_invalid_import_path() {
    let src = "import ;";
    let result = Parser::new("test.mat", src).parse_program();
    assert!(
        result.is_err(),
        "expected parse error for malformed import path"
    );
}

#[test]
fn test_parser_nested_loops_break_continue_ast() {
    let src = r#"
fn main() {
    let mut outer: int = 0;
    while outer < 5 {
        outer++;
        if outer == 2 {
            continue;
        }
        fori (let inner: int = 0; inner < 3; inner++) {
            if inner == 1 {
                break;
            }
        }
    }
}
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("nested_loops_break_continue_ast", prog);
}

#[test]
fn test_parser_vec_function_signatures() {
    let src = r#"
fn process_grid(g: Vec<int>) -> Vec<int> {
    return g;
}
"#;
    assert!(Parser::new("test.mat", src).parse_program().is_ok());
}

#[test]
fn test_parser_multiline_2d_array_literal() {
    let src = r#"
fn main() {
    let grid: [[int; 2]; 2] = [
        [1, 2],
        [3, 4],
    ];
}
"#;
    assert!(Parser::new("test.mat", src).parse_program().is_ok());
}
