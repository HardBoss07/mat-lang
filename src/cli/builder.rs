//! Build and compilation pipeline orchestration.
//!
//! This module coordinates source code parsing, semantic analysis, code generation,
//! artifact emission (LLVM IR, ASM, OBJ), and native linking.

use inkwell::context::Context;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use super::resolver::resolve_and_parse_program;
use crate::Result;
use crate::ast::Program;
use crate::codegen::{CodegenEngine, link_object_file};
use crate::semantic::SemanticAnalyzer;

/// Options configuring target code emission and destination paths.
#[derive(Debug, Clone, Copy)]
pub struct BuildOptions {
    /// Whether to emit text LLVM IR (.ll).
    pub emit_llvm: bool,
    /// Whether to emit assembly source (.s).
    pub emit_asm: bool,
    /// Whether to emit native object file (.o).
    pub emit_obj: bool,
}

/// Orchestrates full compilation from source file to desired target output.
///
/// # Arguments
///
/// * `source` - Path to the root `.mat` source file.
/// * `output` - Optional explicit destination path for the binary or output.
/// * `options` - Emission flags for LLVM IR, assembly, or object files.
///
/// # Returns
///
/// `Ok(())` on successful compilation and linking.
///
/// # Errors
///
/// Returns a [`crate::error::MatcError`] if parsing, typechecking, codegen, or linking fails.
pub fn build_target(source: &Path, output: Option<&Path>, options: BuildOptions) -> Result<()> {
    let source_canonical = source
        .canonicalize()
        .unwrap_or_else(|_| source.to_path_buf());
    let root_dir = source_canonical.parent().unwrap_or_else(|| Path::new("."));

    let mut visited = HashSet::new();
    let items = resolve_and_parse_program(&source_canonical, root_dir, &mut visited)?;
    let ast = Program { items };

    let source_code = fs::read_to_string(source)?;
    let file_name = source.to_string_lossy();

    let mut analyzer = SemanticAnalyzer::new(&file_name, &source_code);
    analyzer.analyze(&ast)?;

    let context = Context::create();
    let module_name = source
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("main_module");
    let codegen = CodegenEngine::new(&context, module_name);
    codegen.compile_program(&ast)?;

    emit_artifacts(source, &codegen, output, options)
}

/// Emits the requested intermediate files and handles final executable linking.
fn emit_artifacts(
    source: &Path,
    codegen: &CodegenEngine,
    output: Option<&Path>,
    options: BuildOptions,
) -> Result<()> {
    let stem = source.file_stem().unwrap_or_default().to_string_lossy();
    let out_dir = PathBuf::from("out");
    fs::create_dir_all(&out_dir)?;

    emit_llvm_if_requested(codegen, &out_dir, &stem, options.emit_llvm)?;
    emit_asm_if_requested(codegen, &out_dir, &stem, options.emit_asm)?;

    let obj_path = out_dir.join(format!("{}.o", stem));
    codegen.write_object_to_file(&obj_path)?;

    if options.emit_obj && output.is_none() {
        println!("Emitted Object File: {}", obj_path.display());
        return Ok(());
    }

    handle_executable_linking(
        &obj_path,
        output,
        &out_dir,
        &stem,
        options.emit_obj,
        options,
    )
}

/// Emits LLVM IR file if requested.
fn emit_llvm_if_requested(
    codegen: &CodegenEngine,
    out_dir: &Path,
    stem: &str,
    emit_llvm: bool,
) -> Result<()> {
    if emit_llvm {
        let llvm_path = out_dir.join(format!("{}.ll", stem));
        codegen.write_llvm_ir_to_file(&llvm_path)?;
        println!("Emitted LLVM IR: {}", llvm_path.display());
    }
    Ok(())
}

/// Emits assembly file if requested.
fn emit_asm_if_requested(
    codegen: &CodegenEngine,
    out_dir: &Path,
    stem: &str,
    emit_asm: bool,
) -> Result<()> {
    if emit_asm {
        let asm_path = out_dir.join(format!("{}.s", stem));
        codegen.write_assembly_to_file(&asm_path)?;
        println!("Emitted Assembly: {}", asm_path.display());
    }
    Ok(())
}

/// Links object file into executable when binary output is desired.
fn handle_executable_linking(
    obj_path: &Path,
    output: Option<&Path>,
    out_dir: &Path,
    stem: &str,
    emit_obj: bool,
    options: BuildOptions,
) -> Result<()> {
    let should_link =
        output.is_some() || (!options.emit_llvm && !options.emit_asm && !options.emit_obj);

    if should_link {
        let default_exec_name = if cfg!(target_os = "windows") {
            format!("{}.exe", stem)
        } else {
            stem.to_string()
        };

        let final_exec_path = output
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| out_dir.join(default_exec_name));

        link_object_file(obj_path, &final_exec_path)?;

        if !emit_obj {
            let _ = fs::remove_file(obj_path);
        }

        println!("Built Executable: {}", final_exec_path.display());
    } else if !emit_obj {
        let _ = fs::remove_file(obj_path);
    }

    Ok(())
}
