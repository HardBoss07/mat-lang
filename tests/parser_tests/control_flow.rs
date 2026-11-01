use crate::common::parse;

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

#[test]
fn test_parser_match_patterns_ast() {
    let src = r#"
fn check(code: int) -> Result<int, String> {
    return Ok(code);
}
fn main() {
    let res: Result<int, String> = check(200);
    match res {
        Ok(v) => { let ok_val: int = v; }
        Err(e) => { let err_val: String = e; }
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
