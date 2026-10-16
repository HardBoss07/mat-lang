//! Function declaration and parameter parsing.
//!
//! This module provides parsers for function signatures, parameters, and bodies.

use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace1;
use winnow::token::literal;

use crate::ast::{FunctionDeclaration, Param, Type};
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::statement::parse_block;
use crate::parser::types::parse_type;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses a function parameter including optional `mut` modifier, identifier, and type.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`Param`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if parameter syntax is invalid.
pub fn parse_param(input: &mut &str) -> ModalResult<Param> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_mutable = parse_param_mut_prefix(input)?;

    let name = match parse_single_identifier_str.parse_next(input) {
        Ok(identifier) => identifier.to_string(),
        Err(err) => {
            *input = start_input;
            return Err(err);
        }
    };
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

    Ok(Param {
        is_mutable,
        name,
        ty,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses an optional `mut` modifier on a function parameter.
fn parse_param_mut_prefix(input: &mut &str) -> ModalResult<bool> {
    if input.starts_with("mut") {
        let after_mut = &input[3..];
        if after_mut
            .chars()
            .next()
            .map_or(false, |c| c.is_whitespace() || c == '_')
        {
            let _ = literal("mut").parse_next(input)?;
            let _ = skip_ws_and_comments(input)?;
            return Ok(true);
        }
    }
    Ok(false)
}

/// Parses a top-level function declaration including signature and body.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`FunctionDeclaration`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if function syntax is invalid.
pub fn parse_function(input: &mut &str) -> ModalResult<FunctionDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = parse_function_pub_prefix(input)?;
    parse_function_with_pub(input, is_pub, start_input)
}

/// Parses optional pub prefix requiring subsequent whitespace for function items.
fn parse_function_pub_prefix(input: &mut &str) -> ModalResult<bool> {
    if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Internal parser for a function declaration when visibility has already been determined.
pub(crate) fn parse_function_with_pub(
    input: &mut &str,
    is_pub: bool,
    start_input: &str,
) -> ModalResult<FunctionDeclaration> {
    if !input.starts_with("fn") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("fn").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    let params = parse_function_param_list(input)?;
    let _ = skip_ws_and_comments(input)?;

    let return_type = parse_function_return_type(input)?;
    let _ = skip_ws_and_comments(input)?;

    let body = parse_block(input)?;
    let end_input = *input;

    Ok(FunctionDeclaration {
        is_pub,
        name,
        params,
        return_type,
        body,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses the parenthesized parameter list of a function.
fn parse_function_param_list(input: &mut &str) -> ModalResult<Vec<Param>> {
    if !input.starts_with('(') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let mut params = Vec::new();
    if !input.starts_with(')') {
        while !input.is_empty() {
            let param = parse_param.parse_next(input)?;
            params.push(param);
            let _ = skip_ws_and_comments(input)?;
            if input.starts_with(',') {
                *input = &input[1..];
                let _ = skip_ws_and_comments(input)?;
                if input.starts_with(')') {
                    break;
                }
            } else {
                break;
            }
        }
    }

    if !input.starts_with(')') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    Ok(params)
}

/// Parses the optional return type annotation (`-> Type`) for a function.
fn parse_function_return_type(input: &mut &str) -> ModalResult<Type> {
    if input.starts_with("->") {
        *input = &input[2..];
        let _ = skip_ws_and_comments(input)?;
        parse_type.parse_next(input)
    } else {
        Ok(Type::Void)
    }
}
