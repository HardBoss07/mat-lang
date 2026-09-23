pub mod expression;
pub mod statement;

use crate::ast::Statement;
use crate::error::Result;

pub struct Parser<'a> {
    pub source: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Statement>> {
        // Winnow parser orchestration
        Ok(Vec::new())
    }
}
