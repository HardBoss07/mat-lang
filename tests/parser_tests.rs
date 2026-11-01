// tests/parser_tests.rs
mod common;

#[path = "parser_tests/control_flow.rs"]
mod control_flow;
#[path = "parser_tests/functions_and_imports.rs"]
mod functions_and_imports;
#[path = "parser_tests/operators_and_collections.rs"]
mod operators_and_collections;
#[path = "parser_tests/structs_and_enums.rs"]
mod structs_and_enums;
#[path = "parser_tests/variables_and_types.rs"]
mod variables_and_types;
