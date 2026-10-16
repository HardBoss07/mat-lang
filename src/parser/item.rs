//! Item parsing module for the MAT language syntax.
//!
//! This module coordinates parsing of top-level language constructs:
//! functions, structs, enums, global variable declarations, and import statements.

pub mod common;
pub mod enum_decl;
pub mod function;
pub mod global_let;
pub mod import_decl;
pub mod struct_decl;

pub use common::*;
pub use enum_decl::*;
pub use function::*;
pub use global_let::*;
pub use import_decl::*;
pub use struct_decl::*;

use winnow::ModalResult;

use crate::ast::Item;
use crate::parser::utils::skip_ws_and_comments;

/// Parses any top-level module item (function, struct, enum, global let, or import).
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Item`] AST node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed or no item matches.
pub fn parse_item(input: &mut &str) -> ModalResult<Item> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    if input.is_empty() {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let is_pub = parse_optional_pub(input)?;

    if input.starts_with("import") {
        parse_item_import(input, is_pub, start_input)
    } else if input.starts_with("struct") {
        parse_item_struct(input, is_pub, start_input)
    } else if input.starts_with("enum") {
        parse_item_enum(input, is_pub, start_input)
    } else if input.starts_with("fn") {
        parse_item_function(input, is_pub, start_input)
    } else if input.starts_with("let") {
        parse_item_global_let(input, is_pub, start_input)
    } else {
        Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ))
    }
}

/// Parses an import item arm, ensuring `pub` is not incorrectly applied to imports.
fn parse_item_import(input: &mut &str, is_pub: bool, start_input: &str) -> ModalResult<Item> {
    if is_pub {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let decl = parse_import_with_start(input, start_input)?;
    Ok(Item::Import(decl))
}

/// Parses a struct item arm.
fn parse_item_struct(input: &mut &str, is_pub: bool, start_input: &str) -> ModalResult<Item> {
    let decl = parse_struct_with_pub(input, is_pub, start_input)?;
    Ok(Item::Struct(decl))
}

/// Parses an enum item arm.
fn parse_item_enum(input: &mut &str, is_pub: bool, start_input: &str) -> ModalResult<Item> {
    let decl = parse_enum_with_pub(input, is_pub, start_input)?;
    Ok(Item::Enum(decl))
}

/// Parses a function item arm.
fn parse_item_function(input: &mut &str, is_pub: bool, start_input: &str) -> ModalResult<Item> {
    let decl = parse_function_with_pub(input, is_pub, start_input)?;
    Ok(Item::Function(decl))
}

/// Parses a global let item arm.
fn parse_item_global_let(input: &mut &str, is_pub: bool, start_input: &str) -> ModalResult<Item> {
    let decl = parse_global_let_with_pub(input, is_pub, start_input)?;
    Ok(Item::GlobalLet(decl))
}
