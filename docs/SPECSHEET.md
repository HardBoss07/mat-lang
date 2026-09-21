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
| `bool`             | Boolean                         | `i1`               |
| `char`             | Unicode / ASCII character       | `i32` / `i8`       |

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
```

## 4. Structs, Tuples, and Arrays

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
        is_active: true,
    };
}
```

### 4.2 Tuples

Heterogeneous sequences indexed with `.0`, `.1`:

```mat
let point: (int, int, bool) = (10, 20, true);
let x: int = point.0;
let is_valid: bool = point.2;
```

### 4.3 Fixed Arrays & Heap Vectors

- **Arrays:** Stack-allocated fixed length (`[int; 4]`).
- **Vectors:** Heap-allocated dynamic sequences (`Vec<T>`).

```mat
// Stack Array
let fixed_nums: [int; 3] = [1, 2, 3];

// Dynamic Vector (Managed by GC)
let mut dynamic_list: Vec<int> = Vec::new();
dynamic_list.push(10);
dynamic_list.push(20);
let first: int = dynamic_list.get(0);
```

## 5. Control Flow

### 5.1 Conditionals (`if` / `else`)

```mat
if (score > 50) {
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
    println("Welcome:");
    println(username);
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
    println(obj.value);
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

4. **Linking Phase:** Passes emitted `.ll` bitcode through `clang` to link against `libgc` (Boehm GC) and standard C system libraries, outputting the final executable file.
