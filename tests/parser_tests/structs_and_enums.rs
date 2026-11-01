use crate::common::parse;
use matc::parser::Parser;

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
    assert!(Parser::new("test.mat", src).parse_program().is_err());
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
