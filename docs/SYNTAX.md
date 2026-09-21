# Syntax Overview

## Key Differentiating Features (`mat` vs. `Rust`)

| Feature / Syntax         | `mat` Syntax                         | Rust Equivalent        | Why it differs                                                                               |
| ------------------------ | ------------------------------------ | ---------------------- | -------------------------------------------------------------------------------------------- |
| **Module Import**        | `import std::io;`                    | `use std::io;`         | Uses standard `import` keyword common in modern languages.                                   |
| **Default Integer**      | `int` (64-bit signed)                | `i64` / `isize`        | `int` is the default 64-bit integer type; fixed-width `i32` is explicit.                     |
| **Pointers & Lifetimes** | _None_ (`String`, `Vec<T>`)          | `&str`, `&mut T`, `'a` | GC-managed heap types eliminate borrow checker syntax, references, and lifetime annotations. |
| **Increment/Decrement**  | `x++;` `x--;`                        | `x += 1;`              | Restores standard C-style unary mutation operators.                                          |
| **Index Loop**           | `fori (let i: int = 0; i < 10; i++)` | `for i in 0..10`       | Explicit 3-part C-style iterator loop keyword (`fori`).                                      |
| **Sequence Loop**        | `for item in items`                  | `for item in &items`   | Iterates directly over values/GC-handles without explicit borrowing (`&`).                   |

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

pub enum Shape {
    Circle(f64),
    Rectangle(Point, Point),
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

    // Unary & Infix Operators
    score += 25;
    score++;

    // Control Flow: Pattern Matching (Expression or Statement)
    let shape_type: Shape = Shape::Circle(5.5);
    match shape_type {
        Shape::Circle(radius) => {
            println("Found circle with radius:");
            println(radius);
        }
        Shape::Rectangle(p1, p2) => {
            println("Found rectangle");
        }
    }

    // Fallible Pattern Matching
    let result: Result<int, String> = parse_coordinate("42");
    match result {
        Ok(val) => println(val);
        Err(err) => println(err);
    }

    // 5. Loops

    // Variant A: C-style Index Loop (fori)
    fori (let i: int = 0; i < 5; i++) {
        score += i;
    }

    // Variant B: Collection Iterator (for in)
    let numbers: [int; 3] = [10, 20, 30];
    for num in numbers {
        println(num);
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

`let` `mut` `fn` `struct` `enum` `import` `pub` `return` `if` `else` `match` `loop` `while` `fori` `for` `in` `break` `continue`

### Built-in Primitive Types

`int` `i32` `i16` `i8` `f64` `bool` `char`

### Operators

- **Arithmetic**: `+` `-` `*` `/` `%`
- **Assignment & Mutation**: `=` `+=` `-=` `*=` `/=` `++` `--`
- **Comparison**: `==` `!=` `<` `<=` `>` `>=`
- **Logical**: `&&` `||` `!`

### Delimiters & Separators

- **Statements**: Terminated with semicolon `;`
- **Blocks**: Enclosed in curly braces `{ }`
- **Tuples & Grouping**: Enclosed in parentheses `( )`
- **Arrays & Indexing**: Enclosed in square brackets `[ ]`
- **Namespace Resolution**: Double colon `::`
- **Field Access**: Single dot `.` (e.g., `tuple.0`, `struct.field`)
