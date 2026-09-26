use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::combinator::{alt, opt};
use winnow::token::literal;

use crate::ast::{Span, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::types::parse_type;

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
