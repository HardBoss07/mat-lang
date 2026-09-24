# Language Specification: **mat** (`.mat`)

## 1. Execution & Runtime Architecture

- **Backend Target:** Direct emission of **LLVM IR** via API bindings (`inkwell` or `llvm-sys` in Rust).
- **Compilation Flow:** Compiles `.mat` files $\rightarrow$ LLVM IR (`.ll`) $\rightarrow$ Optimized LLVM Bitcode $\rightarrow$ Native Object via `clang` / `lld`.
- **Memory Strategy:** Low-latency Garbage Collection via the **Boehm-Demers-Weiser Concurrent GC** (or an Immix mark-region collector).
- Objects on the heap (`String`, `Vec<T>`, Structs allocated via `new`) are managed automatically.
- Allocation uses lock-free thread-local allocation buffers (LABs) to ensure minimal stop-the-world GC pauses, prioritizing execution speed over strict memory footprint efficiency.

## 2. Type System & Pure Function Signatures

**mat** uses a strictly typed, no-exception model. Functions **cannot** implicitly throw runtime exceptions. Everything returned by a function must be explicitly visible in its return type.

### Primitive Types

| Type               | Description                     | LLVM IR Mapping    |
| ------------------ | ------------------------------- | ------------------ |
| `int`              | 64-bit signed integer (default) | `i64`              |
| `i32`, `i16`, `i8` | Fixed-width integers            | `i32`, `i16`, `i8` |
| `f64`              | Double-precision float          | `double`           |
| `bool`             | Boolean (`tru` / `fal`)         | `i1`               |
| `char`             | Unicode / ASCII character       | `i32` / `i8`       |

### Integer & Numeric Literals

**mat** supports decimal, hexadecimal, and binary integer literal formats, with optional underscores `_` as visual digit separators:

- **Decimal:** `100`, `1_000_000`
- **Hexadecimal:** `0xA5`, `0xFF_00_AB`
- **Binary:** `0b10101000`, `0b1100_0011`

```mat
let hex_val: i32 = 0xA5;
let bin_val: i8 = 0b10100100;
let mask: int = 0xFF_00_FF;
```

### String Interpolation

Strings support inline expression and variable interpolation enclosed in curly braces `{}`. String interpolation is resolved at compile time into structured string concatenation and runtime formatting calls.

```mat
let content_var: String = "foobar";
let number: int = 23;

content_var = "foobar {number}";
println("Content: {content_var}");
```

### Error Handling via `Result<T, E>`

Rather than Java-style exceptions or implicit panics, fallible functions return an explicit `Result` type:

```mat
fn parse_number(input: String) -> Result<int, String> {
    if (input.is_empty()) {
        return Err("Input string is empty");
    }
    return Ok(42);
}

fn main() {
    let res: Result<int, String> = parse_number("123");

    // Explicit matching on return status
    match (res) {
        Ok(value) => println(value);
        Err(err)  => println(err);
    }
}
```

## 3. Variables & Infix Mutation

Variables use `let` and statements terminate with `;`.

```mat
let age: int = 18;
let mut score: int = 100;

// Direct inline mutation operators
score += 15;
score -= 5;
score *= 2;
score /= 4;
score++;
score--;

// Bitwise shift operators & short-form compound assignments
let mut mask: int = 0b0001;
mask <<= 3; // Bitwise shift-left assignment (mask becomes 0b1000)
mask >>= 1; // Bitwise shift-right assignment (mask becomes 0b0100)
let shifted: int = mask << 2; // Standard bitwise left-shift
```

## 4. Structs, Tuples, Enums, and Arrays

### 4.1 Structs

Structs are simple data containers allocated on the heap or stack.

```mat
struct Player {
    name: String,
    score: int,
    is_active: bool,
}

fn create_player(name: String) -> Player {
    return Player {
        name: name,
        score: 0,
        is_active: tru,
    };
}
```

### 4.2 Tuples

Heterogeneous sequences indexed with `.0`, `.1`:

```mat
let point: (int, int, bool) = (10, 20, tru);
let x: int = point.0;
let is_valid: bool = point.2;
```

### 4.3 Fixed Arrays & Heap Vectors (1D & 2D)

- **Arrays:** Stack-allocated fixed length (`[int; 4]`, 2D: `[[int; 3]; 2]`).
- **Vectors:** Heap-allocated dynamic sequences (`Vec<T>`, 2D: `Vec<Vec<T>>`).

```mat
// 1D & 2D Stack Arrays
let fixed_nums: [int; 3] = [1, 2, 3];
let grid_2d: [[int; 3]; 2] = [
    [1, 2, 3],
    [4, 5, 6]
];
let cell: int = grid_2d[1][0]; // Access row 1, col 0 (4)

// Dynamic Vector & Nested 2D Vectors (Managed by GC)
let mut dynamic_list: Vec<int> = Vec::new();
dynamic_list.push(10);
dynamic_list.push(20);

let mut matrix_2d: Vec<Vec<int>> = Vec::new();
let mut row1: Vec<int> = Vec::new();
row1.push(1);
row1.push(0);
matrix_2d.push(row1);
let element: int = matrix_2d.get(0).get(0);
```

### 4.4 Enums (Tagged Unions / ADTs)

Enums support rich Rust-like payload variants, including unit variants, tuple-like payloads, and struct-like payloads.

```mat
pub enum NetworkState {
    Idle,
    Connected(String),
    Error(i32, String),
    Data { bytes: Vec<i8>, count: int },
}

fn handle_state(state: NetworkState) {
    match (state) {
        NetworkState::Idle => println("State: Idle");
        NetworkState::Connected(ip) => println("Connected to {ip}");
        NetworkState::Error(code, msg) => println("Error {code}: {msg}");
        NetworkState::Data { bytes, count } => println("Received {count} bytes");
    }
}
```

## 5. Control Flow

### 5.1 Conditionals (`if` / `else`)

Conditionals evaluate boolean expressions that resolve to `tru` or `fal`.

```mat
let is_ready: bool = tru;

if (score > 50 && is_ready == tru) {
    println("High score!");
} else {
    println("Keep trying!");
}
```

### 5.2 Branching / Pattern Matching (`match`)

Replaces switch-case statements with clean expression matching.

```mat
let code: int = 200;

match (code) {
    200 => println("Success");
    404 => println("Not Found");
    _   => println("Unknown Code");
}
```

### 5.3 Loops

**mat** supports four loop variants:

```mat
// 1. Infinite loop with break
let mut k: int = 0;
loop {
    k++;
    if (k > 10) {
        break;
    }
}

// 2. While loop
while (k > 0) {
    k--;
}

// 3. Range-based fori loop
fori (let i: int = 0; i < 10; i++) {
    println(i);
}

// 4. Iterator loop over array or Vec
let items: [int; 3] = [10, 20, 30];
for item in items {
    println(item);
}
```

## 6. System I/O

The standard input and output bindings interface directly with LLVM-compiled runtime functions wrapping standard system descriptor streams:

```mat
fn main() {
    print("Enter username: ");
    let username: String = read_line();
    println("Welcome {username}!");
}
```

## 7. Multi-File Module Resolution

### 7.1 File Structure

Directory structure maps to namespaces using double colons (`::`).

```text
my_project/
├── main.mat
├── math/
│   └── calc.mat
└── utils.mat
```

### 7.2 Imports and Visibility

Functions and structs are private to their `.mat` file unless marked `pub`.

**`math/calc.mat`**

```mat
pub struct Calculation {
    pub value: int,
}

pub fn add(a: int, b: int) -> int {
    return a + b;
}
```

**`main.mat`**

```mat
import math::calc;
import utils;

fn main() {
    let sum: int = calc::add(10, 20);
    let obj: calc::Calculation = calc::Calculation { value: sum };
    println("Calculated result: {obj.value}");
}
```

## 8. Compiler Pipeline & LLVM Code Generation

When compiling **mat** using Rust:

1. **Parser Pass:** Constructs the AST and resolves `import` dependencies recursively from `.mat` files on disk.
2. **Type Pass & Symbol Resolution:** Validates types, checks that mutability flags match assignments, and ensures function return signatures are respected.
3. **LLVM IR Pass:**

- Global names are mangled (`_mat_math_calc_add`).
- Heap types invoke the GC allocator (`@GC_malloc`).
- Local variable mutations generate LLVM `alloca`, `load`, and `store` instructions.
- String interpolation expressions `{expr}` are lowered to dynamic runtime buffer formatting routines (`_mat_rt_fmt_string`).

4. **Linking Phase:** Passes emitted `.ll` bitcode through `clang` to link against `libgc` (Boehm GC) and standard C system libraries, outputting the final executable file.
