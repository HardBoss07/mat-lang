use winnow::ModalResult;
use winnow::Parser;
use winnow::combinator::opt;
use winnow::token::literal;

use crate::ast::Statement;
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::types::parse_type;
use crate::parser::utils::{get_span_between, keyword, skip_ws_and_comments, symbol};

pub fn parse_let_statement(input: &mut &str) -> ModalResult<Statement> {
    let start_input = *input;
    let _ = keyword("let").parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let is_mutable = opt(literal("mut")).parse_next(input)?.is_some();
    let _ = skip_ws_and_comments(input)?;

    let name = parse_identifier_str.parse_next(input)?;
    let _ = skip_ws_and_comments(input)?;

    let ty = if opt(literal(':')).parse_next(input)?.is_some() {
        let parsed_ty = parse_type.parse_next(input)?;
        let _ = skip_ws_and_comments(input)?;
        Some(parsed_ty)
    } else {
        None
    };

    let _ = symbol("=").parse_next(input)?;
    let value = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;
    let end_input = *input;

    Ok(Statement::Let {
        name: name.to_string(),
        is_mutable,
        ty,
        value,
        span: get_span_between(start_input, end_input),
    })
}
