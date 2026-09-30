use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace1;
use winnow::token::literal;

use crate::ast::{FunctionDeclaration, Param, Type};
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::statement::parse_block;
use crate::parser::types::parse_type;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

pub fn parse_param(input: &mut &str) -> ModalResult<Param> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let name = match parse_identifier_str.parse_next(input) {
        Ok(n) => n.to_string(),
        Err(e) => {
            *input = start_input;
            return Err(e);
        }
    };
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with(':') {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let ty = parse_type.parse_next(input)?;
    let end_input = *input;

    Ok(Param {
        name,
        ty,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_function(input: &mut &str) -> ModalResult<FunctionDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with("fn") {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("fn").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_identifier_str.parse_next(input)?.to_string();
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with('(') {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
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
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let _ = skip_ws_and_comments(input)?;

    let return_type = if input.starts_with("->") {
        *input = &input[2..];
        let _ = skip_ws_and_comments(input)?;
        parse_type.parse_next(input)?
    } else {
        Type::Void
    };

    let _ = skip_ws_and_comments(input)?;
    let body = parse_block(input)?;
    let end_input = *input;

    Ok(FunctionDeclaration {
        name,
        params,
        return_type,
        body,
        span: get_span_between(start_input, end_input),
    })
}
