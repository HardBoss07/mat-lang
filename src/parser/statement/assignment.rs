use winnow::ModalResult;
use winnow::Parser;

use crate::ast::{BinaryOp, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

pub fn parse_assignment_target<'a>(input: &mut &'a str) -> ModalResult<String> {
    let _ = skip_ws_and_comments(input)?;
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

pub fn parse_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let target = parse_assignment_target(input)?;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with('=') || input.starts_with("==") {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let value = parse_expression(input)?;
    let _ = skip_ws_and_comments(input)?;

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

pub fn parse_compound_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let target = parse_assignment_target(input)?;
    let _ = skip_ws_and_comments(input)?;

    let (op, op_len) = if input.starts_with("<<=") {
        (BinaryOp::Shl, 3)
    } else if input.starts_with(">>=") {
        (BinaryOp::Shr, 3)
    } else if input.starts_with("+=") {
        (BinaryOp::Add, 2)
    } else if input.starts_with("-=") {
        (BinaryOp::Sub, 2)
    } else if input.starts_with("*=") {
        (BinaryOp::Mul, 2)
    } else if input.starts_with("/=") {
        (BinaryOp::Div, 2)
    } else if input.starts_with("%=") {
        (BinaryOp::Mod, 2)
    } else {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    };

    *input = &input[op_len..];
    let _ = skip_ws_and_comments(input)?;

    let value = parse_expression(input)?;
    let _ = skip_ws_and_comments(input)?;

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

pub fn parse_increment_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let target = parse_assignment_target(input)?;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with("++") {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[2..];
    let _ = skip_ws_and_comments(input)?;

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

pub fn parse_decrement_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let target = parse_assignment_target(input)?;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with("--") {
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[2..];
    let _ = skip_ws_and_comments(input)?;

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
