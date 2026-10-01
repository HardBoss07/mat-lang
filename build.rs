use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=runtime/mat_runtime.c");
    println!("cargo:rerun-if-changed=stdlib");
    println!("cargo:rerun-if-changed=vendor/bdwgc");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_181_PREFIX");
    println!("cargo:rerun-if-env-changed=MATC_FORCE_LLVM_OPT");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();

    // 1. Traverse disk files in stdlib/ and compile embedded_stdlib.rs lookup map
    let stdlib_dir = PathBuf::from("stdlib");
    let mut match_arms = Vec::new();
    if stdlib_dir.exists() {
        collect_std_files(&stdlib_dir, &stdlib_dir, &mut match_arms);
    }

    let embedded_rs_path = out_dir.join("embedded_stdlib.rs");
    let mut embedded_code = String::from(
        "pub fn get_embedded_std_file(key: &str) -> Option<&'static str> {\n    match key {\n",
    );
    for (key, file_path) in match_arms {
        let escaped_path = file_path.to_string_lossy().replace('\\', "/");
        embedded_code.push_str(&format!(
            "        {:?} => Some(include_str!(r{:?})),\n",
            key, escaped_path
        ));
    }
    embedded_code.push_str("        _ => None,\n    }\n}\n");
    fs::write(&embedded_rs_path, embedded_code).expect("Failed to write embedded_stdlib.rs");

    // 2. Build GC and mat_runtime together into one static library
    let mut build = cc::Build::new();
    build
        .file("vendor/bdwgc/extra/gc.c")
        .file("runtime/mat_runtime.c")
        .include("vendor/bdwgc/include")
        .define("GC_BUILTIN_ATOMIC", None)
        .define("GC_NOT_DLL", None)
        .define("GC_THREADS", None)
        .define("MAT_USE_GC", None)
        .define("SMALL_CONFIG", None)
        .define("GC_NO_FINALIZATION", None)
        .define("NO_EXECUTE_PERMISSION", None)
        .define("DONT_ADD_BYTE_AT_END", None)
        .flag("-ffunction-sections")
        .flag("-fdata-sections")
        .flag("-Os")
        .warnings(false);

    if target_os == "windows" {
        build.define("_CRT_SECURE_NO_WARNINGS", None);
        if env::var("CC").is_err() {
            build.compiler("clang");
        }
    }

    build.compile("mat_runtime");

    let lib_name = if target_os == "windows" {
        "mat_runtime.lib"
    } else {
        "libmat_runtime.a"
    };

    let generated_lib_path = out_dir.join(lib_name);
    let embedded_dest_path = out_dir.join("mat_runtime_embedded.bin");

    if generated_lib_path.exists() {
        fs::copy(&generated_lib_path, &embedded_dest_path)
            .expect("Failed to copy combined runtime library for embedding");
    }

    // 3. Compiler Profile Settings
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

fn collect_std_files(base_dir: &Path, current_dir: &Path, acc: &mut Vec<(String, PathBuf)>) {
    if let Ok(entries) = fs::read_dir(current_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_std_files(base_dir, &path, acc);
            } else if path.extension().and_then(|s| s.to_str()) == Some("mat") {
                if let Ok(rel) = path.strip_prefix(base_dir) {
                    let mut components: Vec<_> = rel
                        .components()
                        .map(|c| c.as_os_str().to_string_lossy().to_string())
                        .collect();
                    if let Some(last) = components.last_mut() {
                        *last = last.trim_end_matches(".mat").to_string();
                    }
                    let key = components.join("::");
                    acc.push((key, path.canonicalize().unwrap_or(path)));
                }
            }
        }
    }
}
