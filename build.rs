use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_181_PREFIX");
    println!("cargo:rerun-if-env-changed=MATC_FORCE_LLVM_OPT");

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
