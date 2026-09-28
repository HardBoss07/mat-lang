use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::combinator::alt;

use crate::ast::{Span, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::statement::assignment::{
    parse_assignment_statement, parse_compound_assignment_statement,
};
use crate::parser::statement::declaration::parse_let_statement;
use crate::parser::statement::parse_block;
use crate::parser::utils::{keyword, symbol};

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

pub fn parse_loop_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("loop").parse_next(input)?;
    let body = parse_block(input)?;
    Ok(Statement::Loop {
        body,
        span: Span::new(0, 0),
    })
}

pub fn parse_while_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("while").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let condition = parse_expression.parse_next(input)?;
    let body = parse_block(input)?;
    Ok(Statement::While {
        condition,
        body,
        span: Span::new(0, 0),
    })
}

pub fn parse_fori_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("fori").parse_next(input)?;
    let _ = symbol("(").parse_next(input)?;

    let init = alt((
        parse_let_statement,
        parse_compound_assignment_statement,
        parse_assignment_statement,
    ))
    .parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let condition = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;

    let step = parse_step_statement(input)?;
    let _ = symbol(")").parse_next(input)?;

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
    let _ = keyword("for").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let var_name = parse_identifier_str.parse_next(input)?.to_string();
    let _ = keyword("in").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let iterable = parse_expression.parse_next(input)?;
    let body = parse_block(input)?;
    Ok(Statement::ForIn {
        var_name,
        iterable,
        body,
        span: Span::new(0, 0),
    })
}
