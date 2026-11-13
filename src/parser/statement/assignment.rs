//! Assignment and mutation statement parsing.
//!
//! This module provides parsers for simple assignment (`x = expr`),
//! compound assignment (`x += expr`, `x <<= expr`, etc.),
//! increment (`x++`), and decrement (`x--`) statements.

use winnow::ModalResult;
use winnow::Parser;

use crate::ast::{BinaryOp, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

/// Parses a dotted or scoped assignment target (e.g. `foo.bar`, `Mod::var`).
///
/// Supports chained field access (`.field`), tuple index (`.0`),
/// and module scoping (`::name`).
///
/// # Errors
///
/// Returns a [`winnow::error::ErrMode`] if the initial identifier is invalid.
pub fn parse_assignment_target(input: &mut &str) -> ModalResult<String> {
    skip_ws_and_comments(input)?;
    let start = *input;
    let _ = parse_single_identifier_str(input)?;

    loop {
        let checkpoint = *input;
        if input.starts_with("::") {
            *input = &input[2..];
            if parse_single_identifier_str(input).is_err() {
                *input = checkpoint;
                break;
            }
        } else if input.starts_with('.') {
            *input = &input[1..];
            if parse_single_identifier_str(input).is_err()
                && winnow::ascii::digit1::<_, winnow::error::ContextError>
                    .parse_next(input)
                    .is_err()
            {
                *input = checkpoint;
                break;
            }
        } else {
            break;
        }
    }

    let len = start.len() - input.len();
    Ok(start[..len].to_string())
}

/// Parses a simple assignment statement: `target = value;`.
///
/// # Errors
///
/// Returns a backtrack [`winnow::error::ErrMode`] if the input does not
/// contain a valid `=` (non-`==`) operator or is missing a semicolon.
pub fn parse_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    skip_ws_and_comments(input)?;

    let target = parse_assignment_target(input)?;
    skip_ws_and_comments(input)?;

    if !input.starts_with('=') || input.starts_with("==") {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    skip_ws_and_comments(input)?;

    let value = parse_expression(input)?;
    skip_ws_and_comments(input)?;

    if !input.starts_with(';') {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(Statement::Assignment {
        target,
        value,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses a compound assignment statement: `target op= value;`.
///
/// Supported operators: `+=`, `-=`, `*=`, `/=`, `%=`, `<<=`, `>>=`.
///
/// # Errors
///
/// Returns a backtrack [`winnow::error::ErrMode`] if no compound operator
/// is found or the semicolon is missing.
pub fn parse_compound_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    skip_ws_and_comments(input)?;

    let target = parse_assignment_target(input)?;
    skip_ws_and_comments(input)?;

    let (op, op_len) = parse_compound_operator(input)?;

    *input = &input[op_len..];
    skip_ws_and_comments(input)?;

    let value = parse_expression(input)?;
    skip_ws_and_comments(input)?;

    if !input.starts_with(';') {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(Statement::CompoundAssignment {
        target,
        op,
        value,
        span: get_span_between(start_input, end_input),
    })
}

/// Recognizes a compound assignment operator and returns the corresponding
/// [`BinaryOp`] and the byte length of the operator token.
fn parse_compound_operator(input: &str) -> ModalResult<(BinaryOp, usize)> {
    if input.starts_with("<<=") {
        Ok((BinaryOp::Shl, 3))
    } else if input.starts_with(">>=") {
        Ok((BinaryOp::Shr, 3))
    } else if input.starts_with("+=") {
        Ok((BinaryOp::Add, 2))
    } else if input.starts_with("-=") {
        Ok((BinaryOp::Sub, 2))
    } else if input.starts_with("*=") {
        Ok((BinaryOp::Mul, 2))
    } else if input.starts_with("/=") {
        Ok((BinaryOp::Div, 2))
    } else if input.starts_with("%=") {
        Ok((BinaryOp::Mod, 2))
    } else {
        Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ))
    }
}

/// Parses a postfix increment statement: `target++;`.
///
/// # Errors
///
/// Returns a backtrack [`winnow::error::ErrMode`] if `++` or semicolon is missing.
pub fn parse_increment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    skip_ws_and_comments(input)?;

    let target = parse_assignment_target(input)?;
    skip_ws_and_comments(input)?;

    if !input.starts_with("++") {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[2..];
    skip_ws_and_comments(input)?;

    if !input.starts_with(';') {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(Statement::Increment {
        target,
        span: get_span_between(start_input, end_input),
    })
}

/// Parses a postfix decrement statement: `target--;`.
///
/// # Errors
///
/// Returns a backtrack [`winnow::error::ErrMode`] if `--` or semicolon is missing.
pub fn parse_decrement_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    skip_ws_and_comments(input)?;

    let target = parse_assignment_target(input)?;
    skip_ws_and_comments(input)?;

    if !input.starts_with("--") {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[2..];
    skip_ws_and_comments(input)?;

    if !input.starts_with(';') {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(Statement::Decrement {
        target,
        span: get_span_between(start_input, end_input),
    })
}
