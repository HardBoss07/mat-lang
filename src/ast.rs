//! Abstract Syntax Tree (AST) definitions for the MAT language.
//!
//! This module coordinates the definitions for source spans, language types,
//! expressions, statements, pattern matching, items, and the top-level [`Program`].

pub mod expressions;
pub mod items;
pub mod span;
pub mod statements;
pub mod types;

pub use expressions::*;
pub use items::*;
pub use span::Span;
pub use statements::*;
pub use types::Type;
