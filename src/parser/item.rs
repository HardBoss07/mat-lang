use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace1;
use winnow::token::literal;

use crate::ast::{
    FunctionDeclaration, ImportDeclaration, Param, StructDeclaration, StructField, Type,
};
use crate::parser::expression::primary::parse_single_identifier_str;
use crate::parser::statement::parse_block;
use crate::parser::types::parse_type;
use crate::parser::utils::{get_span_between, skip_ws_and_comments};

pub fn parse_struct_field(input: &mut &str) -> ModalResult<StructField> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let name = match parse_single_identifier_str.parse_next(input) {
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

    Ok(StructField {
        name,
        ty,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_struct(input: &mut &str) -> ModalResult<StructDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        true
    } else {
        false
    };

    if !input.starts_with("struct") {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("struct").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_single_identifier_str.parse_next(input)?.to_string();
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with('{') {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
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
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    *input = &input[1..];
    let end_input = *input;

    Ok(StructDeclaration {
        is_pub,
        name,
        fields,
        span: get_span_between(start_input, end_input),
    })
}

pub fn parse_param(input: &mut &str) -> ModalResult<Param> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let name = match parse_single_identifier_str.parse_next(input) {
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

pub fn parse_import(input: &mut &str) -> ModalResult<ImportDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    if !input.starts_with("import") {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("import").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let is_std = if input.starts_with("std::") {
        *input = &input[5..];
        true
    } else {
        false
    };

    let mut path = Vec::new();
    let first_seg = parse_single_identifier_str.parse_next(input)?.to_string();
    path.push(first_seg);

    while input.starts_with("::") {
        *input = &input[2..];
        let _ = skip_ws_and_comments(input)?;
        let seg = parse_single_identifier_str.parse_next(input)?.to_string();
        path.push(seg);
    }

    let _ = skip_ws_and_comments(input)?;
    if !input.starts_with(';') {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
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

pub fn parse_function(input: &mut &str) -> ModalResult<FunctionDeclaration> {
    let start_input = *input;
    let _ = skip_ws_and_comments(input)?;

    let is_pub = if input.starts_with("pub") {
        let _ = literal("pub").parse_next(input)?;
        let _ = multispace1.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        true
    } else {
        false
    };

    if !input.starts_with("fn") {
        *input = start_input;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("fn").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let name = parse_single_identifier_str.parse_next(input)?.to_string();
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
        is_pub,
        name,
        params,
        return_type,
        body,
        span: get_span_between(start_input, end_input),
    })
}
