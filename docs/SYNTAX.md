# Syntax Overview

## Key Differentiating Features (`mat` vs. `Rust`)

| Feature / Syntax         | `mat` Syntax                         | Rust Equivalent             | Why it differs                                                                               |
| ------------------------ | ------------------------------------ | --------------------------- | -------------------------------------------------------------------------------------------- |
| **Module Import**        | `import std::io;`                    | `use std::io;`              | Uses standard `import` keyword common in modern languages.                                   |
| **Default Integer**      | `int` (64-bit signed)                | `i64` / `isize`             | `int` is the default 64-bit integer type; fixed-width `i32` is explicit.                     |
| **Boolean Literals**     | `tru`, `fal`                         | `true`, `false`             | Concise 3-letter boolean keyword primitives (`tru` / `fal`).                                 |
| **String Interpolation** | `"Hello {name}"`                     | `format!("Hello {}", name)` | First-class string interpolation directly in string literals without formatting macros.      |
| **Pointers & Lifetimes** | _None_ (`String`, `Vec<T>`)          | `&str`, `&mut T`, `'a`      | GC-managed heap types eliminate borrow checker syntax, references, and lifetime annotations. |
| **Increment/Decrement**  | `x++;` `x--;`                        | `x += 1;`                   | Restores standard C-style unary mutation operators.                                          |
| **Bitwise Shift Assign** | `a <<= 3;` `a >>= 1;`                | `a <<= 3;`                  | Short-form compound bitwise shift operators (`<<=`, `>>=`).                                  |
| **Index Loop**           | `fori (let i: int = 0; i < 10; i++)` | `for i in 0..10`            | Explicit 3-part C-style iterator loop keyword (`fori`).                                      |
| **Sequence Loop**        | `for item in items`                  | `for item in &items`        | Iterates directly over values/GC-handles without explicit borrowing (`&`).                   |

## Complete Syntax Showcase (`example.mat`)

```mat
// 1. Module Declarations & Imports
import math::geometry;
import std::io;

// 2. Data Structures (Structs & Enums)
pub struct Point {
    pub x: int,
    pub y: int,
}

// Rust-like Enums with payload types
pub enum Shape {
    Circle(f64),
    Rectangle(Point, Point),
    Label { title: String, is_visible: bool },
}

// 3. Fallible Functions & ADT Return Types
fn parse_coordinate(raw: String) -> Result<int, String> {
    if raw.is_empty() {
        return Err("Cannot parse empty coordinate string");
    }
    return Ok(100);
}

// 4. Main Entry Point
fn main() {
    // Variable Bindings (Immutable by default, mutable with `mut`)
    let origin: Point = Point { x: 0, y: 0 };
    let mut score: int = 50;
    let is_active: bool = tru; // Boolean primitives: tru / fal

    // Binary and Hex Literals
    let hex_mask: i32 = 0xA5;
    let bin_flags: i8 = 0b10100100;

    // String Interpolation
    let mut content_var: String = "foobar";
    let number: int = 23;
    content_var = "foobar {number}";
    println("Content: {content_var}");

    // Unary, Infix, and Bitwise Shift Operators
    score += 25;
    score++;
    let mut mask: int = 0b0001;
    mask <<= 3; // Bitwise left shift short form (0b1000)
    mask >>= 1; // Bitwise right shift short form (0b0100)

    // 2D Array & 2D Vec Definitions
    let grid_2d: [[int; 3]; 2] = [
        [1, 2, 3],
        [4, 5, 6]
    ];
    let grid_val: int = grid_2d[1][0];

    let mut matrix_2d: Vec<Vec<int>> = Vec::new();
    let mut row: Vec<int> = Vec::new();
    row.push(10);
    matrix_2d.push(row);

    // Control Flow: Pattern Matching on Enums (Expression or Statement)
    let shape_type: Shape = Shape::Circle(5.5);
    match shape_type {
        Shape::Circle(radius) => {
            println("Found circle with radius: {radius}");
        }
        Shape::Rectangle(p1, p2) => {
            println("Found rectangle");
        }
        Shape::Label { title, is_visible } => {
            if is_visible == tru {
                println("Visible label: {title}");
            }
        }
    }

    // Fallible Pattern Matching
    let result: Result<int, String> = parse_coordinate("42");
    match result {
        Ok(val)  => println("Parsed value: {val}");
        Err(err) => println("Error: {err}");
    }

    // 5. Loops

    // Variant A: C-style Index Loop (fori)
    fori (let i: int = 0; i < 5; i++) {
        score += i;
    }

    // Variant B: Collection Iterator (for in)
    let numbers: [int; 3] = [10, 20, 30];
    for num in numbers {
        println("Number: {num}");
    }

    // Variant C: Standard While Loop
    let mut countdown: int = 3;
    while countdown > 0 {
        countdown--;
    }

    // Variant D: Infinite Loop with Break
    loop {
        if score > 100 {
            break;
        }
        score += 10;
    }
}
```

## Language Syntax Grammar Quick Reference

### Keywords

`let` `mut` `fn` `struct` `enum` `import` `pub` `return` `if` `else` `match` `loop` `while` `fori` `for` `in` `break` `continue` `tru` `fal`

### Built-in Primitive Types

`int` `i32` `i16` `i8` `f64` `bool` `char`

### Operators

- **Arithmetic**: `+` `-` `*` `/` `%`
- **Bitwise & Shift**: `<<` `>>` `&` `|` `^`
- **Assignment & Mutation**: `=` `+=` `-=` `*=` `/=` `%=` `<<=` `>>=` `++` `--`
- **Comparison**: `==` `!=` `<` `<=` `>` `>=`
- **Logical**: `&&` `||` `!`

### Delimiters & Separators

- **Statements**: Terminated with semicolon `;`
- **Blocks**: Enclosed in curly braces `{ }`
- **Tuples & Grouping**: Enclosed in parentheses `( )`
- **Arrays & Indexing**: Enclosed in square brackets `[ ]`
- **Namespace Resolution**: Double colon `::`
- **Field Access**: Single dot `.` (e.g., `tuple.0`, `struct.field`)
- **String Interpolation Delimiters**: Enclosed in `{ }` inside double quotes `"`
