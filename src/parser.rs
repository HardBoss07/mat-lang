pub mod expression;
pub mod statement;

use winnow::ModalResult;
use winnow::Parser as WinnowParser;
use winnow::ascii::{alphanumeric1, multispace0, multispace1};
use winnow::combinator::{delimited, preceded, repeat};
use winnow::token::literal;

use crate::ast::{FunctionDeclaration, Item, Program, Span, Type};
use crate::error::{MatcError, Result};
use crate::parser::statement::parse_statement;

fn parse_function(input: &mut &str) -> ModalResult<FunctionDeclaration> {
    let _ = multispace0.parse_next(input)?;
    let _ = literal("fn").parse_next(input)?;
    let _ = multispace1.parse_next(input)?;
    let name: &str = alphanumeric1.parse_next(input)?;
    let _ = multispace0.parse_next(input)?;
    let _ = literal("()").parse_next(input)?;
    let _ = multispace0.parse_next(input)?;

    let body: Vec<crate::ast::Statement> = delimited(
        '{',
        repeat(0.., parse_statement),
        preceded(multispace0, '}'),
    )
    .parse_next(input)?;

    Ok(FunctionDeclaration {
        name: name.to_string(),
        return_type: Type::Void,
        body,
        span: Span::new(0, 0),
    })
}

pub struct Parser<'a> {
    source: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut input = self.source;
        let func = parse_function
            .parse_next(&mut input)
            .map_err(|e| MatcError::SyntaxError {
                message: e.to_string(),
                span: (0, 0),
            })?;

        Ok(Program {
            items: vec![Item::Function(func)],
        })
    }
}
