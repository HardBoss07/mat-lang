//! Enum declaration and variant parsing.
//!
//! This module provides parsers for enum definitions and enum variant payloads
//! including unit, tuple-like, and struct-like variants.

use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace1;
use winnow::token::literal;

use super::struct_decl::parse_struct_field;
use crate::ast::{EnumDeclaration, EnumVariant, EnumVariantPayload};
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::types::parse_type;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses an enum declaration item including visibility, variants, and payloads.
///
/// # Arguments
///
/// * `input` - The parser input slice.
///
/// # Returns
///
/// The parsed [`EnumDeclaration`] node.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if enum syntax is invalid.
pub fn parse_enum(input: &mut &str) -> ModalResult<EnumDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = parse_enum_pub_prefix(input)?;
    parse_enum_with_pub(input, is_pub, start_input)
}

/// Parses optional pub prefix requiring subsequent whitespace for enum items.
fn parse_enum_pub_prefix(input: &mut &str) -> ModalResult<bool> {
    if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Internal parser for an enum declaration when visibility has already been determined.
pub(crate) fn parse_enum_with_pub(
    input: &mut &str,
    is_pub: bool,
    start_input: &str,
) -> ModalResult<EnumDeclaration> {
    if !input.starts_with("enum") {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("enum").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    let variants = parse_enum_variant_list(input)?;
    let end_input = *input;

    Ok(EnumDeclaration {
        is_pub,
        name,
        variants,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses the brace-enclosed list of enum variants.
fn parse_enum_variant_list(input: &mut &str) -> ModalResult<Vec<EnumVariant>> {
    if !input.starts_with('{') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let mut variants = Vec::new();
    while !input.is_empty() && !input.starts_with('}') {
        let variant = parse_enum_variant(input)?;
        variants.push(variant);

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
    Ok(variants)
}

/// Parses a single enum variant with name and optional payload.
fn parse_enum_variant(input: &mut &str) -> ModalResult<EnumVariant> {
    let var_start = *input;
    let var_name = parse_single_identifier_str
        .parse_next(input)
        .map_err(|_| winnow::error::ErrMode::Cut(winnow::error::ContextError::default()))?
        .to_string();
    let _ = skip_ws_and_comments(input)?;

    let payload = if input.starts_with('(') {
        parse_tuple_variant_payload(input)?
    } else if input.starts_with('{') {
        parse_struct_variant_payload(input)?
    } else {
        EnumVariantPayload::Unit
    };

    let var_end = *input;
    Ok(EnumVariant {
        name: var_name,
        payload,
        span: get_span_between(var_start, var_end),
    })
}

/// Parses a parenthesized tuple payload for an enum variant.
fn parse_tuple_variant_payload(input: &mut &str) -> ModalResult<EnumVariantPayload> {
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;
    let mut tuple_types = Vec::new();
    if !input.starts_with(')') {
        while !input.is_empty() {
            let ty = parse_type.parse_next(input)?;
            tuple_types.push(ty);
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
    Ok(EnumVariantPayload::Tuple(tuple_types))
}

/// Parses a brace-enclosed struct payload for an enum variant.
fn parse_struct_variant_payload(input: &mut &str) -> ModalResult<EnumVariantPayload> {
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;
    let mut fields = Vec::new();
    if !input.starts_with('}') {
        while !input.is_empty() {
            let field = parse_struct_field.parse_next(input)?;
            fields.push(field);
            let _ = skip_ws_and_comments(input)?;
            if input.starts_with(',') {
                *input = &input[1..];
                let _ = skip_ws_and_comments(input)?;
                if input.starts_with('}') {
                    break;
                }
            } else {
                break;
            }
        }
    }
    if !input.starts_with('}') {
        return Err(winnow::error::ErrMode::Cut(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    Ok(EnumVariantPayload::Struct(fields))
}
