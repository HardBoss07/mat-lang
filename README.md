# Mat Lang (`.mat`)

An experimental programming language built in Rust targeting **LLVM IR 18.1.8**. This project serves as a practical exploration of compiler architecture, AST representation, semantic verification, and native code generation.

## Features

- **Direct LLVM IR Emission:** Targets LLVM 18 IR API bindings via `inkwell`.
- **Flexible Code Generation:** Build native executables, or emit intermediate files (`.ll` LLVM IR, `.s` assembly, `.o` object files).
- **Automatic Toolchain Discovery:** Automatic MSVC runtime and Windows SDK path resolution when linking binaries on Windows.

## Prerequisites

- **LLVM 18.1.8:** Installed with `llvm-config` available in your system path.
  - _Note:_ If you are building LLVM on Windows from source, you can use `build-llvm.ps1`.
- **Clang:** System `clang` executable available in `PATH`.
- **Visual Studio C++ Build Tools (Windows):** MSVC C++ build tools installed (`Desktop development with C++` workload). No special shell or `vcvars64.bat` required.

## Cloning

To clone the repository along with all required submodules in one command:

```bash
git clone --recurse-submodules https://github.com/HardBoss07/mat-lang.git
```

Or, if you have already cloned the repository:

```bash
git submodule update --init --recursive
```

## Usage

### Direct Execution (`run`)

Compiles a `.mat` file directly into the `out/` folder and executes it in a single step:

```bash
cargo run -- run examples/hello-world.mat
```

### Building Binaries & Artifacts (`build`)

Build a native executable (defaults to `out/<filename>`):

```bash
cargo run -- build examples/hello-world.mat
```

Specify a custom output path:

```bash
cargo run -- build examples/hello-world.mat -o out/my_app.exe
```

Emit intermediate IR/assembly artifacts without linking:

```bash
# Emit LLVM IR (.ll)
cargo run -- build examples/hello-world.mat --emit-llvm

# Emit target assembly (.s)
cargo run -- build examples/hello-world.mat --emit-asm

# Emit native object file (.o)
cargo run -- build examples/hello-world.mat --emit-obj
```
