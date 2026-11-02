//! Match and conditional control flow statement parsing.
//!
//! This module provides parsers for `if`/`else` statements, `match` statements,
//! match arms, and the various match pattern forms (wildcard, Ok/Err, enum unit,
//! enum tuple, enum struct, and literal expressions).

use winnow::ModalResult;
use winnow::Parser;

use crate::ast::{MatchArm, MatchPattern, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::statement::parse_block;
use crate::parser::utils::{get_span_between, keyword, skip_ws_and_comments, symbol};

/// Parses a match arm pattern.
///
/// Attempts patterns in the following precedence order:
/// 1. Wildcard (`_`)
/// 2. `Ok(var)` / `Err(var)` result patterns
/// 3. Enum variant patterns (unit, tuple, struct)
/// 4. Literal expression fallback
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if the pattern syntax is invalid.
pub fn parse_match_pattern(input: &mut &str) -> ModalResult<MatchPattern> {
    let _ = skip_ws_and_comments(input)?;

    if symbol("_").parse_next(input).is_ok() {
        return Ok(MatchPattern::Wildcard);
    }

    let checkpoint = *input;

    if let Some(pattern) = try_parse_result_pattern(input, checkpoint)? {
        return Ok(pattern);
    }

    if let Some(pattern) = try_parse_enum_or_identifier_pattern(input, checkpoint)? {
        return Ok(pattern);
    }

    let expr = parse_expression(input)?;
    Ok(MatchPattern::Literal(expr))
}

/// Attempts to parse an `Ok(var)` or `Err(var)` result pattern.
///
/// Returns `None` if the input doesn't start with `Ok` or `Err`.
fn try_parse_result_pattern<'a>(
    input: &mut &'a str,
    checkpoint: &'a str,
) -> ModalResult<Option<MatchPattern>> {
    if keyword("Ok").parse_next(input).is_ok() {
        let _ = skip_ws_and_comments(input)?;
        if symbol("(").parse_next(input).is_ok() {
            let _ = skip_ws_and_comments(input)?;
            let var = parse_identifier_str.parse_next(input)?.to_string();
            let _ = skip_ws_and_comments(input)?;
            let _ = symbol(")").parse_next(input)?;
            return Ok(Some(MatchPattern::Ok(var)));
        }
    }
    *input = checkpoint;

    if keyword("Err").parse_next(input).is_ok() {
        let _ = skip_ws_and_comments(input)?;
        if symbol("(").parse_next(input).is_ok() {
            let _ = skip_ws_and_comments(input)?;
            let var = parse_identifier_str.parse_next(input)?.to_string();
            let _ = skip_ws_and_comments(input)?;
            let _ = symbol(")").parse_next(input)?;
            return Ok(Some(MatchPattern::Err(var)));
        }
    }
    *input = checkpoint;

    Ok(None)
}

/// Attempts to parse an enum variant or scoped identifier pattern.
///
/// Handles unit variants (e.g. `Color::Red`), tuple variants (e.g. `Some(x)`),
/// and struct variants (e.g. `Point { x, y }`).
/// Returns `None` if no identifier is found at the current position.
fn try_parse_enum_or_identifier_pattern<'a>(
    input: &mut &'a str,
    checkpoint: &'a str,
) -> ModalResult<Option<MatchPattern>> {
    if let Ok(first_id) = parse_identifier_str.parse_next(input) {
        let _ = skip_ws_and_comments(input)?;
        let (enum_name, variant) = if symbol("::").parse_next(input).is_ok() {
            let _ = skip_ws_and_comments(input)?;
            let var_id = parse_identifier_str.parse_next(input)?;
            (Some(first_id.to_string()), var_id.to_string())
        } else {
            (None, first_id.to_string())
        };

        let _ = skip_ws_and_comments(input)?;

        if let Some(pattern) = try_parse_tuple_payload(input, &enum_name, &variant)? {
            return Ok(Some(pattern));
        }

        if let Some(pattern) = try_parse_struct_payload(input, &enum_name, &variant)? {
            return Ok(Some(pattern));
        }

        if enum_name.is_some() || variant.chars().next().map_or(false, |c| c.is_uppercase()) {
            return Ok(Some(MatchPattern::EnumUnit { enum_name, variant }));
        }

        *input = checkpoint;
    } else {
        *input = checkpoint;
    }

    Ok(None)
}

/// Parses an optional tuple-style payload `(var1, var2, ...)` for an enum pattern.
fn try_parse_tuple_payload(
    input: &mut &str,
    enum_name: &Option<String>,
    variant: &str,
) -> ModalResult<Option<MatchPattern>> {
    if symbol("(").parse_next(input).is_ok() {
        let mut vars = Vec::new();
        let _ = skip_ws_and_comments(input)?;
        if !input.starts_with(')') {
            loop {
                let variable = parse_identifier_str.parse_next(input)?.to_string();
                vars.push(variable);
                let _ = skip_ws_and_comments(input)?;
                if symbol(",").parse_next(input).is_err() {
                    break;
                }
                let _ = skip_ws_and_comments(input)?;
            }
        }
        let _ = symbol(")").parse_next(input)?;
        return Ok(Some(MatchPattern::EnumTuple {
            enum_name: enum_name.clone(),
            variant: variant.to_string(),
            vars,
        }));
    }
    Ok(None)
}

/// Parses an optional struct-style payload `{ field: binding, ... }` for an enum pattern.
fn try_parse_struct_payload(
    input: &mut &str,
    enum_name: &Option<String>,
    variant: &str,
) -> ModalResult<Option<MatchPattern>> {
    if symbol("{").parse_next(input).is_ok() {
        let mut fields = Vec::new();
        let _ = skip_ws_and_comments(input)?;
        if !input.starts_with('}') {
            loop {
                let field = parse_identifier_str.parse_next(input)?.to_string();
                let _ = skip_ws_and_comments(input)?;
                let binding = if symbol(":").parse_next(input).is_ok() {
                    let _ = skip_ws_and_comments(input)?;
                    parse_identifier_str.parse_next(input)?.to_string()
                } else {
                    field.clone()
                };
                fields.push((field, binding));
                let _ = skip_ws_and_comments(input)?;
                if symbol(",").parse_next(input).is_err() {
                    break;
                }
                let _ = skip_ws_and_comments(input)?;
            }
        }
        let _ = symbol("}").parse_next(input)?;
        return Ok(Some(MatchPattern::EnumStruct {
            enum_name: enum_name.clone(),
            variant: variant.to_string(),
            fields,
        }));
    }
    Ok(None)
}

/// Parses a single match arm: `pattern => body`.
///
/// The body may be either a brace-enclosed block or a single statement.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if the arm pattern or arrow syntax is invalid.
pub fn parse_match_arm(input: &mut &str) -> ModalResult<MatchArm> {
    let _ = skip_ws_and_comments(input)?;
    let pattern = parse_match_pattern(input)?;
    let _ = skip_ws_and_comments(input)?;
    let _ = symbol("=>").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let body = if input.starts_with('{') {
        parse_block(input)?
    } else {
        let stmt = crate::parser::statement::parse_statement(input)?;
        vec![stmt]
    };

    Ok(MatchArm { pattern, body })
}

/// Parses an `if` statement with optional `else` / `else if` chains.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if the if-statement syntax is invalid.
pub fn parse_if_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("if").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let condition = parse_expression.parse_next(input)?;
    let then_branch = parse_block(input)?;
    let _ = skip_ws_and_comments(input)?;

    let else_branch = if keyword("else").parse_next(input).is_ok() {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with("if") {
            let nested_if = parse_if_statement(input)?;
            Some(vec![nested_if])
        } else {
            Some(parse_block(input)?)
        }
    } else {
        None
    };

    let end_input = *input;
    Ok(Statement::If {
        condition,
        then_branch,
        else_branch,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses a `match` statement with brace-enclosed arms.
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if the match syntax is invalid.
pub fn parse_match_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("match").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let expr = parse_expression.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;
    let _ = symbol("{").parse_next(input)?;

    let mut arms = Vec::new();
    loop {
        let _ = skip_ws_and_comments(input)?;
        if input.starts_with('}') {
            break;
        }
        let arm = parse_match_arm(input)?;
        arms.push(arm);
    }

    let _ = symbol("}").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Match {
        expr,
        arms,
        span: get_span_between(start_input, end_input),
    })
}
