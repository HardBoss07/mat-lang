// tests/codegen_tests.rs
mod common;

#[path = "codegen_tests/control_flow_ir.rs"]
mod control_flow_ir;
#[path = "codegen_tests/data_structures_ir.rs"]
mod data_structures_ir;
#[path = "codegen_tests/operators_and_calls.rs"]
mod operators_and_calls;

use common::{normalize_ir, parse};
use inkwell::context::Context;
use matc::codegen::CodegenEngine;

pub fn emit_ir(src: &str) -> String {
    let program = parse(src);
    let mut analyzer = matc::semantic::SemanticAnalyzer::new("test.mat", src);
    let _ = analyzer.analyze(&program);

    let context = Context::create();
    let engine = CodegenEngine::new(&context, "test_module");
    engine
        .compile_program(&program)
        .expect("codegen should succeed for valid program");
    normalize_ir(&engine.emit_llvm_ir())
}
