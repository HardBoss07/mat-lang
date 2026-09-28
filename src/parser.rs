pub mod expression;
pub mod item;
pub mod statement;
pub mod types;
pub mod utils;

pub use expression::parse_expression;
pub use item::parse_function;
pub use statement::parse_statement;
pub use types::parse_type;

use winnow::ModalResult;
use winnow::Parser as WinnowParser;
use winnow::ascii::multispace0;

use crate::ast::{Item, Program};
use crate::error::{MatcError, Result};

pub struct Parser<'a> {
    source: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut input = self.source;
        let mut items = Vec::new();

        while !input.trim().is_empty() {
            let _: ModalResult<&str> = multispace0.parse_next(&mut input);
            if input.trim().is_empty() {
                break;
            }
            let func =
                parse_function
                    .parse_next(&mut input)
                    .map_err(|e| MatcError::SyntaxError {
                        message: e.to_string(),
                        span: (0, 0),
                    })?;
            items.push(Item::Function(func));
        }

        Ok(Program { items })
    }
}
