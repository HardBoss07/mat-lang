use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::error::{MatcError, Result};

const RUNTIME_LIB_BYTES: &[u8] =
    include_bytes!(concat!(env!("OUT_DIR"), "/mat_runtime_embedded.bin"));

pub fn link_object_file(obj_path: &Path, output_path: &Path) -> Result<()> {
    let ext = if cfg!(target_os = "windows") {
        "lib"
    } else {
        "a"
    };
    let runtime_lib_path = obj_path.with_file_name(format!("mat_runtime.{}", ext));

    fs::write(&runtime_lib_path, RUNTIME_LIB_BYTES).map_err(|e| {
        MatcError::CodegenError(format!("Failed to write runtime library file: {}", e))
    })?;

    let mut cmd = Command::new("clang");
    cmd.arg(obj_path)
        .arg(&runtime_lib_path)
        .arg("-o")
        .arg(output_path);

    if cfg!(target_os = "windows") {
        cmd.arg("-fuse-ld=lld-link");
        cmd.arg("-luser32");
        cmd.arg("-ladvapi32");

        for lib_path in find_msvc_lib_paths() {
            cmd.arg(format!("-L{}", lib_path.display()));
        }
    } else {
        cmd.arg("-lpthread");
        cmd.arg("-ldl");
    }

    let status = cmd
        .status()
        .map_err(|e| MatcError::CodegenError(format!("Failed to execute clang linker: {}", e)))?;

    let _ = fs::remove_file(&runtime_lib_path);

    if !status.success() {
        return Err(MatcError::CodegenError(format!(
            "Linker failed with exit status: {:?}",
            status.code()
        )));
    }

    Ok(())
}

#[cfg(target_os = "windows")]
fn find_msvc_lib_paths() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    if let Ok(lib_env) = std::env::var("LIB") {
        for path in std::env::split_paths(&lib_env) {
            if path.exists() {
                paths.push(path);
            }
        }
        if !paths.is_empty() {
            return paths;
        }
    }

    let vswhere_path =
        PathBuf::from(r"C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe");
    let mut search_roots = Vec::new();

    if vswhere_path.exists() {
        if let Ok(output) = Command::new(&vswhere_path)
            .args(&[
                "-products",
                "*",
                "-requires",
                "Microsoft.VisualStudio.Component.VC.Tools.x86.x64",
                "-property",
                "installationPath",
            ])
            .output()
        {
            if output.status.success() {
                let stdout = String::from_utf8_lossy(&output.stdout);
                for line in stdout.lines() {
                    let trimmed = line.trim();
                    if !trimmed.is_empty() {
                        search_roots.push(PathBuf::from(trimmed));
                    }
                }
            }
        }
    }

    search_roots.extend(vec![
        PathBuf::from(r"C:\Program Files\Microsoft Visual Studio\2022\Community"),
        PathBuf::from(r"C:\Program Files\Microsoft Visual Studio\2022\BuildTools"),
        PathBuf::from(r"C:\Program Files\Microsoft Visual Studio\2022\Professional"),
        PathBuf::from(r"C:\Program Files\Microsoft Visual Studio\2022\Enterprise"),
        PathBuf::from(r"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community"),
        PathBuf::from(r"C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools"),
    ]);

    for root in search_roots {
        let msvc_base = root.join("VC").join("Tools").join("MSVC");
        if msvc_base.exists() {
            if let Ok(entries) = fs::read_dir(&msvc_base) {
                let mut versions: Vec<_> = entries
                    .filter_map(|e| e.ok())
                    .map(|e| e.path())
                    .filter(|p| p.is_dir())
                    .collect();
                versions.sort();
                if let Some(latest) = versions.last() {
                    let lib_x64 = latest.join("lib").join("x64");
                    if lib_x64.exists() {
                        paths.push(lib_x64);
                        break;
                    }
                }
            }
        }
    }

    let sdk_base = PathBuf::from(r"C:\Program Files (x86)\Windows Kits\10\Lib");
    if sdk_base.exists() {
        if let Ok(entries) = fs::read_dir(&sdk_base) {
            let mut versions: Vec<_> = entries
                .filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| p.is_dir())
                .collect();
            versions.sort();
            if let Some(latest) = versions.last() {
                let ucrt_x64 = latest.join("ucrt").join("x64");
                let um_x64 = latest.join("um").join("x64");
                if ucrt_x64.exists() {
                    paths.push(ucrt_x64);
                }
                if um_x64.exists() {
                    paths.push(um_x64);
                }
            }
        }
    }

    paths
}

#[cfg(not(target_os = "windows"))]
fn find_msvc_lib_paths() -> Vec<PathBuf> {
    Vec::new()
}
