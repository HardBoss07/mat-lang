mod binary_ops;
mod expr_checker;
mod stmt_checker;

use super::symbol_table::SymbolTable;
use crate::ast::{Item, Program, Span};
use crate::error::{MatcError, Result};
use miette::SourceSpan;

pub struct TypeChecker {
    pub file_name: String,
    pub source: String,
}

impl TypeChecker {
    pub fn new(file_name: &str, source: &str) -> Self {
        Self {
            file_name: file_name.to_string(),
            source: source.to_string(),
        }
    }

    pub fn empty() -> Self {
        Self {
            file_name: "<unknown>".to_string(),
            source: String::new(),
        }
    }

    pub fn check_program(&self, program: &Program, symbols: &mut SymbolTable) -> Result<()> {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    let param_tys = func.params.iter().map(|p| p.ty.clone()).collect();
                    symbols.insert_function(func.name.clone(), param_tys, func.return_type.clone());
                }
                Item::Import(_) => {}
            }
        }

        for item in &program.items {
            match item {
                Item::Function(func) => {
                    symbols.push_scope();
                    for param in &func.params {
                        symbols.insert(param.name.clone(), param.ty.clone(), false);
                    }
                    for stmt in &func.body {
                        self.check_statement(stmt, &func.return_type, symbols)?;
                    }
                    symbols.pop_scope();
                }
                Item::Import(_) => {}
            }
        }
        Ok(())
    }

    pub(crate) fn type_error(&self, message: impl Into<String>, span: Span) -> MatcError {
        MatcError::type_error(
            &self.file_name,
            &self.source,
            message,
            SourceSpan::from(span),
        )
    }

    pub(crate) fn undefined_variable(&self, name: impl Into<String>, span: Span) -> MatcError {
        MatcError::undefined_variable(&self.file_name, &self.source, name, SourceSpan::from(span))
    }

    pub(crate) fn undefined_function(&self, name: impl Into<String>, span: Span) -> MatcError {
        MatcError::undefined_function(&self.file_name, &self.source, name, SourceSpan::from(span))
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::empty()
    }
}
