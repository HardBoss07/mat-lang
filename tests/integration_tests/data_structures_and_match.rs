use crate::assert_output;
use crate::common::compile_and_run;

#[test]
fn test_e2e_tuple_access() {
    let src = r#"
fn main() {
    let t: (int, bool, f64) = (99, tru, 3.14);
    println(t.0); println(t.1);
}
"#;
    let output = compile_and_run(src);
    assert!(output.contains("99"));
    assert!(output.contains("tru"));
}

#[test]
fn test_e2e_array_index() {
    let src = r#"
fn main() {
    let arr: [int; 5] = [10, 20, 30, 40, 50];
    println(arr[0]); println(arr[4]);
    let mut sum: int = 0;
    fori (let i: int = 0; i < 5; i++) { sum += arr[i]; }
    println(sum);
}
"#;
    assert_output(src, "10\n50\n150");
}

#[test]
fn test_e2e_result_and_match() {
    let src = r#"
fn safe_div(a: int, b: int) -> Result<int, String> {
    if b == 0 { return Err("division by zero"); }
    return Ok(a / b);
}
fn main() {
    let r1: Result<int, String> = safe_div(20, 4);
    match r1 { Ok(v) => println(v); Err(e) => println(e); }
    let r2: Result<int, String> = safe_div(5, 0);
    match r2 { Ok(v) => println(v); Err(e) => println(e); }
}
"#;
    assert_output(src, "5\ndivision by zero");
}

#[test]
fn test_e2e_multi_function() {
    let src = r#"
fn double(n: int) -> int { return n * 2; }
fn square(n: int) -> int { return n * n; }
fn main() {
    println(double(6));
    println(square(7));
    println(double(square(3)));
}
"#;
    assert_output(src, "12\n49\n18");
}

#[test]
fn test_e2e_struct_instantiation_and_field_access() {
    let src = r#"
struct Point { x: int, y: int }
fn double_point(p: Point) -> Point { return Point { x: p.x * 2, y: p.y * 2 }; }
fn main() {
    let p1: Point = Point { x: 15, y: 25 };
    println(p1.x); println(p1.y);
    let p2: Point = double_point(p1);
    println(p2.x); println(p2.y);
}
"#;
    assert_output(src, "15\n25\n30\n50");
}

#[test]
fn test_e2e_nested_structs() {
    let src = r#"
struct Vector2 { x: int, y: int }
struct Player { pos: Vector2, hp: int }
fn main() {
    let p: Player = Player { pos: Vector2 { x: 100, y: 200 }, hp: 100 };
    println(p.pos.x); println(p.pos.y); println(p.hp);
}
"#;
    assert_output(src, "100\n200\n100");
}

#[test]
fn test_e2e_enum_unit_tuple_struct_variants() {
    let src = r#"
enum Shape { Empty, Circle(int), Rectangle { width: int, height: int } }
fn inspect(s: Shape) {
    match s {
        Shape::Empty => println("empty");
        Shape::Circle(r) => println("circle: {r}");
        Shape::Rectangle { width, height } => println("rect: {width}x{height}");
    }
}
fn main() {
    inspect(Shape::Empty);
    inspect(Shape::Circle(15));
    inspect(Shape::Rectangle { width: 10, height: 20 });
}
"#;
    assert_output(src, "empty\ncircle: 15\nrect: 10x20");
}

#[test]
fn test_e2e_vec_of_structs_field_read() {
    let src = r#"
struct Item { id: int, val: f64 }
fn main() {
    let mut v: Vec<Item> = Vec::new();
    v.push(Item { id: 42, val: 3.14 });
    let item: Item = v[0];
    println(item.id); println(item.val);
}
"#;
    assert_output(src, "42\n3.14");
}
