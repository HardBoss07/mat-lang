pub mod expression;
pub mod item;
pub mod statement;
pub mod types;
pub mod utils;

pub use expression::parse_expression;
pub use item::{
    parse_enum, parse_function, parse_global_let, parse_import, parse_item, parse_struct,
};
pub use statement::parse_statement;
pub use types::parse_type;

use winnow::Parser as WinnowParser;

use crate::ast::Program;
use crate::error::{MatcError, Result};
use crate::parser::utils::{set_root_source, skip_ws_and_comments};

pub struct Parser<'a> {
    file_name: String,
    source: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(file_name: &str, source: &'a str) -> Self {
        Self {
            file_name: file_name.to_string(),
            source,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        set_root_source(self.source);
        let mut input = self.source;
        let mut items = Vec::new();

        while !input.trim().is_empty() {
            let _ = skip_ws_and_comments(&mut input);
            if input.trim().is_empty() {
                break;
            }
            let item = parse_item.parse_next(&mut input).map_err(|e| {
                let err_offset = self.source.len() - input.len();
                MatcError::syntax_error(
                    &self.file_name,
                    self.source,
                    e.to_string(),
                    (err_offset, 1),
                )
            })?;
            items.push(item);
        }

        Ok(Program { items })
    }
}
