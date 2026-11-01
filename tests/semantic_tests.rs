// tests/semantic_tests.rs
mod common;

#[path = "semantic_tests/control_flow_and_functions.rs"]
mod control_flow_and_functions;
#[path = "semantic_tests/negative_tests.rs"]
mod negative_tests;
#[path = "semantic_tests/structs_enums_collections.rs"]
mod structs_enums_collections;
#[path = "semantic_tests/types_and_variables.rs"]
mod types_and_variables;
