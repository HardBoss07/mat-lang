use crate::common::parse;

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
fn test_parser_multiline_2d_array_literal() {
    let src = r#"
fn main() {
    let grid: [[int; 2]; 2] = [
        [1, 2],
        [3, 4],
    ];
}
"#;
    assert!(
        matc::parser::Parser::new("test.mat", src)
            .parse_program()
            .is_ok()
    );
}
