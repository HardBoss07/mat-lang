use winnow::ModalResult;
use winnow::Parser;

use crate::ast::{BinaryOp, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::utils::{get_span_between, skip_ws_and_comments, symbol};

pub fn parse_compound_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let target = match parse_identifier_str.parse_next(input) {
        Ok(t) => t,
        Err(e) => {
            *input = start_input;
            return Err(e);
        }
    };
    let _ = skip_ws_and_comments(input)?;

    let op = if input.starts_with("+=") {
        *input = &input[2..];
        BinaryOp::Add
    } else if input.starts_with("-=") {
        *input = &input[2..];
        BinaryOp::Sub
    } else if input.starts_with("*=") {
        *input = &input[2..];
        BinaryOp::Mul
    } else if input.starts_with("/=") {
        *input = &input[2..];
        BinaryOp::Div
    } else if input.starts_with("%=") {
        *input = &input[2..];
        BinaryOp::Mod
    } else if input.starts_with("<<=") {
        *input = &input[3..];
        BinaryOp::Shl
    } else if input.starts_with(">>=") {
        *input = &input[3..];
        BinaryOp::Shr
    } else {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    };

    let value = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::CompoundAssignment {
        target: target.to_string(),
        op,
        value,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let target = parse_identifier_str.parse_next(input)?;
    let _ = symbol("=").parse_next(input)?;
    let value = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Assignment {
        target: target.to_string(),
        value,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_increment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let target = parse_identifier_str.parse_next(input)?;
    let _ = symbol("++").parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Increment {
        target: target.to_string(),
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_decrement_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let target = parse_identifier_str.parse_next(input)?;
    let _ = symbol("--").parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Decrement {
        target: target.to_string(),
        span: get_span_between(start_input, end_input),
    })
}
