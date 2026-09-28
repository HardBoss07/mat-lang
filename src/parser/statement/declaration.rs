use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::combinator::opt;
use winnow::token::literal;

use crate::ast::{Span, Statement};
use crate::parser::expression::parse_expression;
use crate::parser::expression::primary::parse_identifier_str;
use crate::parser::types::parse_type;
use crate::parser::utils::{keyword, symbol};

pub fn parse_let_statement(input: &mut &str) -> ModalResult<Statement> {
    let _ = keyword("let").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let is_mutable = opt(literal("mut")).parse_next(input)?.is_some();
    let _ = multispace0.parse_next(input)?;

    let name = parse_identifier_str.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let ty = if opt(literal(':')).parse_next(input)?.is_some() {
        let parsed_ty = parse_type.parse_next(input)?;
        let _ = multispace0.parse_next(input)?;
        Some(parsed_ty)
    } else {
        None
    };

    let _ = symbol("=").parse_next(input)?;
    let value = parse_expression.parse_next(input)?;
    let _ = symbol(";").parse_next(input)?;

    Ok(Statement::Let {
        name: name.to_string(),
        is_mutable,
        ty,
        value,
        span: Span::new(0, 0),
    })
}
