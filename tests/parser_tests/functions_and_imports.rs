use crate::common::parse;
use matc::parser::Parser;

#[test]
fn test_parser_functions_ast() {
    let src = r#"
fn no_args() { let x: int = 1; }
fn one_arg(n: int) -> int { return n; }
fn multi_args(a: int, b: bool, c: f64) -> f64 { return c; }
fn factorial(n: int) -> int {
    if n <= 1 { return 1; }
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

#[test]
fn test_parser_import_declarations_ast() {
    let src = r#"
import std::io;
import std::math;
import geometry::calc;

fn main() { let x: int = 10; }
"#;
    let prog = parse(src);
    insta::assert_yaml_snapshot!("import_declarations_ast", prog);
}

#[test]
fn test_parser_error_invalid_import_path() {
    let src = "import ;";
    assert!(Parser::new("test.mat", src).parse_program().is_err());
}

#[test]
fn test_parser_vec_function_signatures() {
    let src = "fn process_grid(g: Vec<int>) -> Vec<int> { return g; }";
    assert!(Parser::new("test.mat", src).parse_program().is_ok());
}

#[test]
fn test_parser_error_missing_fn_parens() {
    let src = "fn main { let x: int = 1; }\n";
    assert!(Parser::new("test.mat", src).parse_program().is_err());
}

#[test]
fn test_parser_error_missing_semicolon() {
    let src = "fn main() { let x: int = 1 }\n";
    assert!(Parser::new("test.mat", src).parse_program().is_err());
}

#[test]
fn test_parser_error_unclosed_block() {
    let src = "fn main() { let x: int = 1;\n";
    assert!(Parser::new("test.mat", src).parse_program().is_err());
}

#[test]
fn test_parser_error_bad_type_annotation() {
    let src = "fn main() { let x: !!bool = tru; }\n";
    assert!(Parser::new("test.mat", src).parse_program().is_err());
}

#[test]
fn test_parser_error_bad_token_in_expression() {
    let src = "fn main() { let x: int = @5; }\n";
    assert!(Parser::new("test.mat", src).parse_program().is_err());
}
