use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::combinator::{alt, opt};
use winnow::token::literal;

use crate::ast::{BinaryOp, Span, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::types::parse_type;

pub fn parse_block(input: &mut &str) -> ModalResult<Vec<Statement>> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with('{') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let _ = literal('{').parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let mut statements = Vec::new();
    while !input.starts_with('}') && !input.is_empty() {
        let stmt = match parse_statement.parse_next(input) {
            Ok(s) => s,
            Err(e) => {
                *input = checkpoint;
                return Err(e);
            }
        };
        statements.push(stmt);
        let _ = multispace0.parse_next(input)?;
    }

    if !input.starts_with('}') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal('}').parse_next(input)?;

    Ok(statements)
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

pub fn parse_if_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("if") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let after_if = &input[2..];
    if after_if
        .chars()
        .next()
        .map_or(false, |c| c.is_alphanumeric() || c == '_')
    {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }

    let _ = literal("if").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let condition = match parse_expression.parse_next(input) {
        Ok(c) => c,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    let then_branch = match parse_block(input) {
        Ok(b) => b,
        Err(e) => {
            *input = checkpoint;
            return Err(e);
        }
    };
    let _ = multispace0.parse_next(input)?;

    let else_branch = if input.starts_with("else") {
        let after_else = &input[4..];
        if !after_else
            .chars()
            .next()
            .map_or(false, |c| c.is_alphanumeric() || c == '_')
        {
            let _ = literal("else").parse_next(input)?;
            let _ = multispace0.parse_next(input)?;
            if input.starts_with("if") {
                match parse_if_statement(input) {
                    Ok(stmt) => Some(vec![stmt]),
                    Err(e) => {
                        *input = checkpoint;
                        return Err(e);
                    }
                }
            } else {
                match parse_block(input) {
                    Ok(b) => Some(b),
                    Err(e) => {
                        *input = checkpoint;
                        return Err(e);
                    }
                }
            }
        } else {
            None
        }
    } else {
        None
    };

    Ok(Statement::If {
        condition,
        then_branch,
        else_branch,
        span: Span::new(0, 0),
    })
}

pub fn parse_compound_assignment_statement(input: &mut &str) -> ModalResult<Statement> {
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
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    };

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

    Ok(Statement::CompoundAssignment {
        target: target.to_string(),
        op,
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

pub fn parse_loop_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("loop") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("loop").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let body = parse_block(input)?;
    Ok(Statement::Loop {
        body,
        span: Span::new(0, 0),
    })
}

pub fn parse_while_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("while") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("while").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let condition = parse_expression.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let body = parse_block(input)?;
    Ok(Statement::While {
        condition,
        body,
        span: Span::new(0, 0),
    })
}

fn parse_step_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    let target = parse_identifier_str.parse_next(input)?.to_string();
    let _ = multispace0.parse_next(input)?;

    if input.starts_with("++") {
        *input = &input[2..];
        Ok(Statement::Increment {
            target,
            span: Span::new(0, 0),
        })
    } else if input.starts_with("--") {
        *input = &input[2..];
        Ok(Statement::Decrement {
            target,
            span: Span::new(0, 0),
        })
    } else if input.starts_with("+=")
        || input.starts_with("-=")
        || input.starts_with("*=")
        || input.starts_with("/=")
        || input.starts_with("%=")
    {
        *input = checkpoint;
        let mut compound = parse_compound_assignment_statement(input)?;
        if let Statement::CompoundAssignment { ref mut span, .. } = compound {
            *span = Span::new(0, 0);
        }
        Ok(compound)
    } else if input.starts_with('=') {
        *input = checkpoint;
        let mut assign = parse_assignment_statement(input)?;
        if let Statement::Assignment { ref mut span, .. } = assign {
            *span = Span::new(0, 0);
        }
        Ok(assign)
    } else {
        *input = checkpoint;
        Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ))
    }
}

pub fn parse_fori_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("fori") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("fori").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with('(') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal('(').parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let init = alt((
        parse_let_statement,
        parse_compound_assignment_statement,
        parse_assignment_statement,
    ))
    .parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let condition = parse_expression.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(';') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(';').parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let step = parse_step_statement(input)?;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(')') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(')').parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let body = parse_block(input)?;
    Ok(Statement::ForI {
        init: Box::new(init),
        condition,
        step: Box::new(step),
        body,
        span: Span::new(0, 0),
    })
}

pub fn parse_for_in_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("for") || input.starts_with("fori") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("for").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let var_name = parse_identifier_str.parse_next(input)?.to_string();
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("in") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("in").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let iterable = parse_expression.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let body = parse_block(input)?;
    Ok(Statement::ForIn {
        var_name,
        iterable,
        body,
        span: Span::new(0, 0),
    })
}

pub fn parse_break_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("break") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("break").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(';') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Break(Span::new(0, 0)))
}

pub fn parse_continue_statement(input: &mut &str) -> ModalResult<Statement> {
    let checkpoint = *input;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with("continue") {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal("continue").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    if !input.starts_with(';') {
        *input = checkpoint;
        return Err(winnow::error::ErrMode::Backtrack(
            winnow::error::ContextError::default(),
        ));
    }
    let _ = literal(';').parse_next(input)?;

    Ok(Statement::Continue(Span::new(0, 0)))
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
        parse_if_statement,
        parse_loop_statement,
        parse_while_statement,
        parse_fori_statement,
        parse_for_in_statement,
        parse_break_statement,
        parse_continue_statement,
        parse_increment_statement,
        parse_decrement_statement,
        parse_compound_assignment_statement,
        parse_assignment_statement,
        parse_expression_statement,
    ))
    .parse_next(input)
}
