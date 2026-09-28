// tests/parser_tests.rs
//
// Structural AST snapshot tests for the mat-lang parser.
// Each test focuses on a distinct grammar domain. Spans are stripped from
// snapshots via `#[serde(skip)]` on AST span fields.

mod common;

use common::parse;
use matc::ast::Item;
use matc::ast::Statement;
use matc::parser::Parser;

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------
fn body_of(src: &str) -> Vec<Statement> {
    match parse(src).items.into_iter().next().unwrap() {
        Item::Function(f) => f.body,
    }
}

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
// 10. Negative syntax error tests — parser must fail gracefully
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
