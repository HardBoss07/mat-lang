// tests/common/mod.rs
// Shared test helpers for the mat-lang integration test suite.

#![allow(dead_code)]

use std::process::Command;
use tempfile::tempdir;

use matc::ast::Program;
use matc::error::Result as MatResult;
use matc::parser::Parser;
use matc::semantic::SemanticAnalyzer;

// ---------------------------------------------------------------------------
// Parsing helpers
// ---------------------------------------------------------------------------

/// Parse `src` into a Program. Panics with a clear message on parse failure.
pub fn parse(src: &str) -> Program {
    Parser::new(src)
        .parse_program()
        .expect("parse() called in test should not fail")
}

/// Parse `src` and run semantic analysis. Returns the Result so negative tests
/// can inspect the error variant and message.
pub fn check(src: &str) -> MatResult<()> {
    let program = Parser::new(src)
        .parse_program()
        .expect("source in check() should parse cleanly");
    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&program)
}

// ---------------------------------------------------------------------------
// LLVM IR normalization helper (used by codegen_tests)
// ---------------------------------------------------------------------------

/// Strip lines that vary across host platforms so IR snapshots are portable.
pub fn normalize_ir(ir: &str) -> String {
    ir.lines()
        .filter(|l| {
            !l.starts_with("; ModuleID")
                && !l.starts_with("source_filename")
                && !l.starts_with("target datalayout")
                && !l.starts_with("target triple")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

// ---------------------------------------------------------------------------
// End-to-end compilation and execution
// ---------------------------------------------------------------------------

/// Write `src` to a temp `.mat` file, invoke the `matc` binary to compile it
/// into a native executable inside a temp directory, run the executable, and
/// return its captured stdout as a `String`.
pub fn compile_and_run(src: &str) -> String {
    let dir = tempdir().expect("tempdir should be creatable");

    // Write source file inside tempdir.
    let src_path = dir.path().join("test_program.mat");
    std::fs::write(&src_path, src).expect("should write .mat source");

    // Resolve the compiled matc binary from CARGO_MANIFEST_DIR.
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let matc_bin = std::path::PathBuf::from(manifest_dir)
        .join("target")
        .join("debug")
        .join(if cfg!(target_os = "windows") {
            "matc.exe"
        } else {
            "matc"
        });

    // Compile output binary path inside tempdir.
    let exec_name = if cfg!(target_os = "windows") {
        "test_out.exe"
    } else {
        "test_out"
    };
    let exec_path = dir.path().join(exec_name);

    // Set current_dir to tempdir so matc emits intermediate files inside the isolated temp folder
    let compile_output = Command::new(&matc_bin)
        .current_dir(dir.path())
        .args([
            "build",
            src_path.to_str().unwrap(),
            "-o",
            exec_path.to_str().unwrap(),
        ])
        .output()
        .expect("matc should be executable");

    assert!(
        compile_output.status.success(),
        "matc compilation failed for source:\n{}\n\nstderr:\n{}",
        src,
        String::from_utf8_lossy(&compile_output.stderr)
    );

    // Run compiled binary inside isolated tempdir.
    let output = Command::new(&exec_path)
        .current_dir(dir.path())
        .output()
        .expect("compiled binary should be runnable");

    assert!(
        output.status.success(),
        "compiled binary exited with non-zero status. stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Normalize Windows CRLF line endings to LF (\n) so output string comparisons pass
    String::from_utf8_lossy(&output.stdout).replace("\r\n", "\n")
}
