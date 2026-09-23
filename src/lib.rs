pub mod ast;
pub mod cli;
pub mod codegen;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod semantic;

pub use error::{MatcError, Result};
