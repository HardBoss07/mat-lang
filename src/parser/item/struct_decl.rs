//! Struct and struct field declaration parsing.
//!
//! This module provides parsers for struct declarations and individual struct fields.

use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace1;
use winnow::token::literal;

use super::common::parse_optional_pub;
use crate::ast::{StructDeclaration, StructField};
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::types::parse_type;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses an individual struct field declaration with optional visibility, identifier, and type.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`StructField`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if syntax is malformed.
pub fn parse_struct_field(input: &mut &str) -> ModalResult<StructField> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = parse_optional_pub(input)?;

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with(':') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let ty = parse_type.parse_next(input)?;
    let end_input = *input;

    Ok(StructField {
        is_pub,
        name,
        ty,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses a struct declaration item including visibility, keyword, name, and field body.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`StructDeclaration`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if the struct syntax is invalid.
pub fn parse_struct(input: &mut &str) -> ModalResult<StructDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = parse_struct_pub_prefix(input)?;
    parse_struct_with_pub(input, is_pub, start_input)
}

/// Parses optional pub prefix requiring subsequent whitespace for struct items.
fn parse_struct_pub_prefix(input: &mut &str) -> ModalResult<bool> {
    if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Internal parser for a struct declaration when visibility has already been determined.
pub(crate) fn parse_struct_with_pub(
    input: &mut &str,
    is_pub: bool,
    start_input: &str,
) -> ModalResult<StructDeclaration> {
    if !input.starts_with("struct") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("struct").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    let fields = parse_struct_field_list(input)?;
    let end_input = *input;

    Ok(StructDeclaration {
        is_pub,
        name,
        fields,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses the brace-enclosed comma-separated list of struct fields.
fn parse_struct_field_list(input: &mut &str) -> ModalResult<Vec<StructField>> {
    if !input.starts_with('{') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let mut fields = Vec::new();
    while !input.is_empty() && !input.starts_with('}') {
        let field = parse_struct_field.parse_next(input)?;
        fields.push(field);
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with(',') {
            *input = &input[1..];
            let _ = skip_ws_and_comments(input)?;
        } else {
            break;
        }
    }

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with('}') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    Ok(fields)
}
