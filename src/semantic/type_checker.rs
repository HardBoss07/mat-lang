use super::symbol_table::SymbolTable;
use crate::ast::{Expression, Item, Program, Statement, Type};
use crate::error::{MatcError, Result};

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check_program(&self, program: &Program, symbols: &mut SymbolTable) -> Result<()> {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    for stmt in &func.body {
                        self.check_statement(stmt, symbols)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn check_statement(&self, stmt: &Statement, symbols: &mut SymbolTable) -> Result<()> {
        match stmt {
            Statement::Let {
                name,
                is_mutable,
                ty,
                value,
                ..
            } => {
                let val_type = self.infer_expression_type(value, symbols)?;
                if val_type != *ty {
                    return Err(MatcError::TypeError {
                        message: format!(
                            "Type mismatch for variable '{}': expected {:?}, got {:?}",
                            name, ty, val_type
                        ),
                    });
                }
                symbols.insert(name.clone(), ty.clone(), *is_mutable);
            }
            Statement::Expression(expr) => {
                self.infer_expression_type(expr, symbols)?;
            }
        }
        Ok(())
    }

    fn infer_expression_type(&self, expr: &Expression, symbols: &SymbolTable) -> Result<Type> {
        match expr {
            Expression::Identifier(name, _) => {
                let sym = symbols.lookup(name).ok_or_else(|| MatcError::TypeError {
                    message: format!("Undefined variable: {}", name),
                })?;
                Ok(sym.ty.clone())
            }
            Expression::IntLiteral(_, _) => Ok(Type::Int),
            Expression::BoolLiteral(_, _) => Ok(Type::Bool),
            Expression::StringLiteral(_, _) => Ok(Type::String),
            Expression::InterpolatedString(parts, _) => {
                for part in parts {
                    self.infer_expression_type(part, symbols)?;
                }
                Ok(Type::String)
            }
            Expression::Call {
                callee, arguments, ..
            } => {
                if symbols.lookup(callee).is_none() {
                    return Err(MatcError::TypeError {
                        message: format!("Undefined function: {}", callee),
                    });
                }
                for arg in arguments {
                    self.infer_expression_type(arg, symbols)?;
                }
                Ok(Type::Void)
            }
        }
    }
}
