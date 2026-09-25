use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::{digit1, multispace0};
use winnow::combinator::{alt, delimited, opt, separated};
use winnow::token::literal;

use crate::ast::{Span, Statement, Type};
use crate::parser::expression::{parse_expression, parse_identifier_str};

pub fn parse_type(input: &mut &str) -> ModalResult<Type> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if input.starts_with('(') {
        let tuple_res: ModalResult<Vec<Type>> =
            delimited('(', separated(0.., parse_type, ','), ')').parse_next(input);
        if let Ok(types) = tuple_res {
            return Ok(Type::Tuple(types));
        }
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    if input.starts_with('[') {
        let mut inner_cp = *input;
        inner_cp = &inner_cp[1..];
        if let Ok(elem_type) = parse_type.parse_next(&mut inner_cp) {
            let _ = multispace0.parse_next(&mut inner_cp)?;
            if inner_cp.starts_with(';') {
                inner_cp = &inner_cp[1..];
                let _ = multispace0.parse_next(&mut inner_cp)?;
                let len_res: ModalResult<&str> = digit1.parse_next(&mut inner_cp);
                if let Ok(len_str) = len_res {
                    if let Ok(len) = len_str.parse::<usize>() {
                        let _ = multispace0.parse_next(&mut inner_cp)?;
                        if inner_cp.starts_with(']') {
                            inner_cp = &inner_cp[1..];
                            *input = inner_cp;
                            return Ok(Type::Array(Box::new(elem_type), len));
                        }
                    }
                }
            }
        }
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    if let Ok(type_str) = parse_identifier_str.parse_next(input) {
        match type_str {
            "int" => return Ok(Type::Int),
            "i32" => return Ok(Type::I32),
            "i16" => return Ok(Type::I16),
            "i8" => return Ok(Type::I8),
            "f64" => return Ok(Type::F64),
            "f32" => return Ok(Type::F32),
            "bool" => return Ok(Type::Bool),
            "String" => return Ok(Type::String),
            other => return Ok(Type::Custom(other.to_string())),
        }
    }

    *input = checkpoint;
    Err(winnow::error::ErrMode::Backtrack(
        winnow::error::ContextError::default(),
    ))
}

pub fn parse_let_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("let") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("let").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let is_mutable = opt(literal("mut")).parse_next(input)?.is_some();
    let _ = multispace0.parse_next(input)?;

    let name = match parse_identifier_str.parse_next(input) {
        Ok(n) => n,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    let ty = if opt(literal(':')).parse_next(input)?.is_some() {
        let parsed_ty = match parse_type.parse_next(input) {
            Ok(t) => t,
            Err(e) => {
                *input = checkpoint;
                return Err(e);
            }
        };
        let _ = multispace0.parse_next(input)?;
        Some(parsed_ty)
    } else {
        None
    };

    if !input.starts_with('=') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal('=').parse_next(input)?;
    let value = match parse_expression.parse_next(input) {
        Ok(v) => v,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(';') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Let {
        name: name.to_string(),
        is_mutable,
        ty,
        value,
        span: Span::new(0, 0),
    })
}

pub fn parse_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    let target = match parse_identifier_str.parse_next(input) {
        Ok(t) => t,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with('=') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal('=').parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let value = match parse_expression.parse_next(input) {
        Ok(v) => v,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(';') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Assignment {
        target: target.to_string(),
        value,
        span: Span::new(0, 0),
    })
}

pub fn parse_increment_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    let target = match parse_identifier_str.parse_next(input) {
        Ok(t) => t,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("++") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("++").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(';') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Increment {
        target: target.to_string(),
        span: Span::new(0, 0),
    })
}

pub fn parse_decrement_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    let target = match parse_identifier_str.parse_next(input) {
        Ok(t) => t,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("--") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("--").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(';') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Decrement {
        target: target.to_string(),
        span: Span::new(0, 0),
    })
}

pub fn parse_expression_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    let expr = match parse_expression.parse_next(input) {
        Ok(e) => e,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(';') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Expression(expr))
}

pub fn parse_statement(input: &mut &str) -> ModalResult<Statement> {
    alt((
        parse_let_statement,
        parse_increment_statement,
        parse_decrement_statement,
        parse_assignment_statement,
        parse_expression_statement,
    ))
    .parse_next(input)
}
