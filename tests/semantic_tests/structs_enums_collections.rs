use crate::common::check;

fn assert_ok(src: &str) {
    check(src).unwrap_or_else(|e| panic!("expected Ok but got error: {e}"));
}

#[test]
fn test_semantic_valid_for_in_array() {
    assert_ok(
        r#"
fn main() {
    let nums: [int; 4] = [1, 2, 3, 4];
    for n in nums { let v: int = n; }
    let flags: [bool; 2] = [tru, fal];
    for f in flags { let b: bool = f; }
}
"#,
    );
}

#[test]
fn test_semantic_valid_result_match() {
    assert_ok(
        r#"
fn divide(a: int, b: int) -> Result<int, String> {
    if b == 0 { return Err("division by zero"); }
    return Ok(a / b);
}
fn main() {
    let res: Result<int, String> = divide(10, 2);
    match res {
        Ok(v)  => { let x: int = v; }
        Err(e) => { let s: String = e; }
    }
}
"#,
    );
}

#[test]
fn test_semantic_valid_tuple_access_and_array_index() {
    assert_ok(
        r#"
fn main() {
    let t: (int, bool, f64) = (5, tru, 3.14);
    let x: int = t.0;
    let b: bool = t.1;
    let f: f64 = t.2;
    let arr: [int; 3] = [10, 20, 30];
    let elem: int = arr[0];
}
"#,
    );
}

#[test]
fn test_semantic_valid_struct_usage() {
    assert_ok(
        r#"
struct Point { x: int, y: int }
struct Rectangle { origin: Point, width: int, height: int }
fn get_x(p: Point) -> int { return p.x; }
fn main() {
    let p: Point = Point { x: 5, y: 10 };
    let x_val: int = get_x(p);
    let rect: Rectangle = Rectangle { origin: p, width: 100, height: 50 };
    let rect_x: int = rect.origin.x;
}
"#,
    );
}

#[test]
fn test_semantic_valid_3d_array_indexing() {
    assert_ok(
        r#"
fn main() {
    let cube: [[[int; 2]; 2]; 2] = [
        [[1, 2], [3, 4]],
        [[5, 6], [7, 8]]
    ];
    let elem: int = cube[0][1][0];
}
"#,
    );
}

#[test]
fn test_semantic_valid_enum_payload_matching() {
    assert_ok(
        r#"
enum Command {
    Quit,
    Move(int, int),
    Write { text: String },
}

fn main() {
    let cmd: Command = Command::Move(10, 20);
    match cmd {
        Command::Quit => { let q: int = 1; }
        Command::Move(x, y) => { let sum: int = x + y; }
        Command::Write { text } => { let s: String = text; }
    }
}
"#,
    );
}
