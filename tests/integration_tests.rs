// tests/integration_tests.rs
mod common;

#[path = "integration_tests/basic_and_formatting.rs"]
mod basic_and_formatting;
#[path = "integration_tests/control_flow_and_recursion.rs"]
mod control_flow_and_recursion;
#[path = "integration_tests/data_structures_and_match.rs"]
mod data_structures_and_match;
#[path = "integration_tests/multi_file_and_stdlib.rs"]
mod multi_file_and_stdlib;

pub fn assert_output(src: &str, expected: &str) {
    let output = common::compile_and_run(src);
    let got = output.trim_end_matches(['\n', '\r']);
    let want = expected.trim_end_matches(['\n', '\r']);
    assert_eq!(
        got, want,
        "\nExpected output:\n{}\n\nActual output:\n{}",
        want, got
    );
}
