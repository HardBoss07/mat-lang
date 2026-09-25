use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=runtime/mat_runtime.c");
    println!("cargo:rerun-if-changed=vendor/bdwgc");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_181_PREFIX");
    println!("cargo:rerun-if-env-changed=MATC_FORCE_LLVM_OPT");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();

    // 1. Build Boehm GC statically using cc (Fix B)
    let mut gc_build = cc::Build::new();
    gc_build
        .file("vendor/bdwgc/extra/gc.c")
        .include("vendor/bdwgc/include")
        // Force native compiler atomics (Interlocked* on MSVC / __atomic_* on GCC/Clang)
        .define("GC_BUILTIN_ATOMIC", None)
        // Prevent dllimport declarations for static linking
        .define("GC_NOT_DLL", None)
        // Enable multithreading support (replaces enable_threads=ON)
        .define("GC_THREADS", None)
        .warnings(false);

    if target_os == "windows" {
        gc_build.define("_CRT_SECURE_NO_WARNINGS", None);
        if env::var("CC").is_err() {
            gc_build.compiler("clang");
        }
    }

    gc_build.compile("gc");

    let gc_lib_name = if target_os == "windows" {
        "gc.lib"
    } else {
        "libgc.a"
    };

    let gc_lib_path = out_dir.join(gc_lib_name);
    let gc_embedded_path = out_dir.join("gc_embedded.bin");

    if gc_lib_path.exists() {
        fs::copy(&gc_lib_path, &gc_embedded_path)
            .expect("Failed to copy compiled GC library for embedding");
    }

    // 2. Compile runtime/mat_runtime.c with access to vendor/bdwgc/include
    let mut runtime_build = cc::Build::new();
    runtime_build
        .file("runtime/mat_runtime.c")
        .include("vendor/bdwgc/include")
        .define("GC_BUILTIN_ATOMIC", None)
        .define("GC_NOT_DLL", None)
        .define("GC_THREADS", None)
        .warnings(false);

    if target_os == "windows" {
        runtime_build.define("_CRT_SECURE_NO_WARNINGS", None);
        if env::var("CC").is_err() {
            runtime_build.compiler("clang");
        }
    }

    runtime_build.compile("mat_runtime");

    let generated_lib_name = if target_os == "windows" {
        "mat_runtime.lib"
    } else {
        "libmat_runtime.a"
    };

    let generated_lib_path = out_dir.join(generated_lib_name);
    let embedded_dest_path = out_dir.join("mat_runtime_embedded.bin");

    if generated_lib_path.exists() {
        fs::copy(&generated_lib_path, &embedded_dest_path)
            .expect("Failed to copy compiled runtime library for embedding");
    }

    // 3. Compiler Feature & Target Flags
    let profile = env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
    let opt_level = env::var("OPT_LEVEL").unwrap_or_else(|_| "0".to_string());

    match profile.as_str() {
        "debug" | "test" => {
            println!("cargo:rustc-cfg=dev_build");

            if env::var("MATC_FORCE_LLVM_OPT").is_err() && opt_level == "0" {
                println!("cargo:rustc-cfg=skip_llvm_opt_passes");
            }
        }
        "release" => {
            println!("cargo:rustc-cfg=release_build");
        }
        _ => {}
    }

    if let Ok(llvm_path) = env::var("LLVM_SYS_181_PREFIX") {
        let lib_dir = PathBuf::from(&llvm_path).join("lib");
        if lib_dir.exists() {
            println!("cargo:rustc-link-search=native={}", lib_dir.display());
        }
    }
}
