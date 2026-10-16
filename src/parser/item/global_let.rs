//! Global variable declaration parsing.
//!
//! This module provides parsers for module-level `let` and `let mut` variable bindings.

use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace1;
use winnow::token::literal;

use crate::ast::GlobalLetDeclaration;
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::types::parse_type;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses a top-level global let declaration statement.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`GlobalLetDeclaration`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_global_let(input: &mut &str) -> ModalResult<GlobalLetDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = parse_global_let_pub_prefix(input)?;
    parse_global_let_with_pub(input, is_pub, start_input)
}

/// Parses optional pub prefix requiring subsequent whitespace for global let items.
fn parse_global_let_pub_prefix(input: &mut &str) -> ModalResult<bool> {
    if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Internal parser for a global let declaration when visibility has already been determined.
pub(crate) fn parse_global_let_with_pub(
    input: &mut &str,
    is_pub: bool,
    start_input: &str,
) -> ModalResult<GlobalLetDeclaration> {
    if !input.starts_with("let") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("let").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let is_mutable = parse_optional_mut(input)?;

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    let ty = parse_optional_type_annotation(input)?;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with('=') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let value = parse_expression(input)?;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with(';') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(GlobalLetDeclaration {
        is_pub,
        name,
        is_mutable,
        ty,
        value,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses an optional `mut` modifier following `let`.
fn parse_optional_mut(input: &mut &str) -> ModalResult<bool> {
    if input.starts_with("mut") {
        let _ = literal("mut").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Parses an optional `: Type` annotation.
fn parse_optional_type_annotation(input: &mut &str) -> ModalResult<Option<crate::ast::Type>> {
    if input.starts_with(':') {
        *input = &input[1..];
        let _ = skip_ws_and_comments(input)?;
        Ok(Some(parse_type.parse_next(input)?))
    } else {
        Ok(None)
    }
}
