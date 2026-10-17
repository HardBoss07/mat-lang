mod binary_ops;
mod expr_checker;
mod stmt_checker;

use super::symbol_table::{EnumVariantSymbol, StructFieldSymbol, SymbolTable};
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
                Item::Struct(struct_decl) => {
                    let fields: Vec<StructFieldSymbol> = struct_decl
                        .fields
                        .iter()
                        .map(|f| StructFieldSymbol {
                            is_pub: f.is_pub,
                            name: f.name.clone(),
                            ty: f.ty.clone(),
                        })
                        .collect();
                    symbols.insert_struct(struct_decl.name.clone(), fields);
                }
                Item::Enum(enum_decl) => {
                    let variants = enum_decl
                        .variants
                        .iter()
                        .map(|v| EnumVariantSymbol {
                            name: v.name.clone(),
                            payload: v.payload.clone(),
                        })
                        .collect();
                    symbols.insert_enum(enum_decl.name.clone(), variants);
                }
                Item::GlobalLet(global_let) => {
                    let ty = if let Some(annotated_ty) = &global_let.ty {
                        annotated_ty.clone()
                    } else {
                        self.synthesize_expr(&global_let.value, symbols)?
                    };
                    symbols.insert(global_let.name.clone(), ty, global_let.is_mutable);
                }
                Item::Function(func) => {
                    let param_tys = func.params.iter().map(|p| p.ty.clone()).collect();
                    symbols.insert_function(func.name.clone(), param_tys, func.return_type.clone());
                }
                Item::Import(_) => {}
            }
        }

        for item in &program.items {
            match item {
                Item::GlobalLet(global_let) => {
                    if let Some(expected_ty) = &global_let.ty {
                        self.check_expr(&global_let.value, expected_ty, symbols)?;
                    } else {
                        self.synthesize_expr(&global_let.value, symbols)?;
                    }
                }
                Item::Function(func) => {
                    symbols.push_scope();
                    for param in &func.params {
                        symbols.insert(param.name.clone(), param.ty.clone(), false);
                    }
                    for stmt in &func.body {
                        self.check_statement(stmt, &func.return_type, symbols)?;
                    }

                    symbols.verify_no_unresolved_infer_types(|var_name| {
                        let span = func.body.last().map(|s| s.span()).unwrap_or_else(|| Span::new(0, 0));
                        self.type_error(
                            format!(
                                "Type inference failed for variable '{}': explicit type annotation required",
                                var_name
                            ),
                            span,
                        )
                    })?;

                    symbols.pop_scope();
                }
                Item::Struct(_) | Item::Enum(_) | Item::Import(_) => {}
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
