//! Import declaration parsing.
//!
//! This module provides parsers for `import` statements including stdlib and local paths.

use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace1;
use winnow::token::literal;

use crate::ast::ImportDeclaration;
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses an import declaration statement.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`ImportDeclaration`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if import syntax is invalid.
pub fn parse_import(input: &mut &str) -> ModalResult<ImportDeclaration> {
    let start_input = *input;
    parse_import_with_start(input, start_input)
}

/// Internal parser for an import declaration given its original start slice.
pub(crate) fn parse_import_with_start(
    input: &mut &str,
    start_input: &str,
) -> ModalResult<ImportDeclaration> {
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with("import") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("import").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let is_std = parse_import_std_prefix(input);
    let path = parse_import_segments(input)?;

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with(';') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(ImportDeclaration {
        is_std,
        path,
        span: get_span_between(start_input, end_input),
    })
}

/// Checks and consumes an optional `std::` prefix on the import target.
fn parse_import_std_prefix(input: &mut &str) -> bool {
    if input.starts_with("std::") {
        *input = &input[5..];
        true
    } else {
        false
    }
}

/// Parses the double-colon delimited path segments of the import statement.
fn parse_import_segments(input: &mut &str) -> ModalResult<Vec<String>> {
    let mut path = Vec::new();
    let first_segment = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    path.push(first_segment);

    while input.starts_with("::") {
        *input = &input[2..];
        let _ = skip_ws_and_comments(input)?;
        let segment = parse_single_identifier_str
            .parse_next(input)
            .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
            .to_string();
        path.push(segment);
    }
    Ok(path)
}
