use super::super::symbol_table::SymbolTable;
use super::TypeChecker;
use crate::ast::{Expression, Type};
use crate::error::{MatcError, Result};

impl TypeChecker {
    pub fn synthesize_expr(&self, expr: &Expression, symbols: &SymbolTable) -> Result<Type> {
        match expr {
            Expression::Identifier(name, _) => {
                let sym = symbols.lookup(name).ok_or_else(|| MatcError::TypeError {
                    message: format!("Undefined variable: {}", name),
                })?;
                Ok(sym.ty.clone())
            }
            Expression::IntLiteral(_, _) => Ok(Type::Int),
            Expression::FloatLiteral(_, _) => Ok(Type::F64),
            Expression::BoolLiteral(_, _) => Ok(Type::Bool),
            Expression::StringLiteral(_, _) => Ok(Type::String),
            Expression::InterpolatedString(parts, _) => {
                for (part, _specifier) in parts {
                    self.synthesize_expr(part, symbols)?;
                }
                Ok(Type::String)
            }
            Expression::Binary {
                op, left, right, ..
            } => self.synthesize_binary_expr(op, left, right, symbols),
            Expression::TupleLiteral(elements, _) => {
                let mut types = Vec::new();
                for elem in elements {
                    types.push(self.synthesize_expr(elem, symbols)?);
                }
                Ok(Type::Tuple(types))
            }
            Expression::ArrayLiteral(elements, _) => {
                if elements.is_empty() {
                    return Err(MatcError::TypeError {
                        message:
                            "Cannot infer type of empty array literal without explicit annotation"
                                .to_string(),
                    });
                }
                let first_ty = self.synthesize_expr(&elements[0], symbols)?;
                for elem in &elements[1..] {
                    let elem_ty = self.synthesize_expr(elem, symbols)?;
                    if elem_ty != first_ty {
                        return Err(MatcError::TypeError {
                            message: format!(
                                "Mismatched types in array literal: expected {:?}, got {:?}",
                                first_ty, elem_ty
                            ),
                        });
                    }
                }
                Ok(Type::Array(Box::new(first_ty), elements.len()))
            }
            Expression::TupleAccess { expr, index, .. } => {
                let expr_ty = self.synthesize_expr(expr, symbols)?;
                match expr_ty {
                    Type::Tuple(types) => {
                        types
                            .get(*index)
                            .cloned()
                            .ok_or_else(|| MatcError::TypeError {
                                message: format!(
                                    "Tuple index {} out of bounds for tuple length {}",
                                    index,
                                    types.len()
                                ),
                            })
                    }
                    other => Err(MatcError::TypeError {
                        message: format!("Cannot access tuple index on non-tuple type {:?}", other),
                    }),
                }
            }
            Expression::ArrayAccess { expr, index, .. } => {
                let expr_ty = self.synthesize_expr(expr, symbols)?;
                let index_ty = self.synthesize_expr(index, symbols)?;
                if index_ty != Type::Int && index_ty != Type::I32 {
                    return Err(MatcError::TypeError {
                        message: format!("Array index must be an integer, got {:?}", index_ty),
                    });
                }
                match expr_ty {
                    Type::Array(elem_ty, _) => Ok(*elem_ty),
                    other => Err(MatcError::TypeError {
                        message: format!("Cannot index non-array type {:?}", other),
                    }),
                }
            }
            Expression::Call {
                callee, arguments, ..
            } => {
                let fn_sym = symbols
                    .lookup_function(callee)
                    .ok_or_else(|| MatcError::TypeError {
                        message: format!("Undefined function: {}", callee),
                    })?
                    .clone();

                if callee != "println" && arguments.len() != fn_sym.param_types.len() {
                    return Err(MatcError::TypeError {
                        message: format!(
                            "Function '{}' expects {} arguments, got {}",
                            callee,
                            fn_sym.param_types.len(),
                            arguments.len()
                        ),
                    });
                }

                if callee != "println" {
                    for (arg, param_ty) in arguments.iter().zip(fn_sym.param_types.iter()) {
                        self.check_expr(arg, param_ty, symbols)?;
                    }
                } else {
                    for arg in arguments {
                        self.synthesize_expr(arg, symbols)?;
                    }
                }
                Ok(fn_sym.return_type)
            }
            Expression::Ok(_, _) | Expression::Err(_, _) => Err(MatcError::TypeError {
                message: "Ok(...) and Err(...) constructors require type context".to_string(),
            }),
        }
    }

    pub fn check_expr(
        &self,
        expr: &Expression,
        target: &Type,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        match (expr, target) {
            (Expression::Ok(val, _), Type::Result(ok_ty, _)) => {
                self.check_expr(val, ok_ty, symbols)?;
                Ok(target.clone())
            }
            (Expression::Err(val, _), Type::Result(_, err_ty)) => {
                self.check_expr(val, err_ty, symbols)?;
                Ok(target.clone())
            }
            (Expression::IntLiteral(val, _), target_ty) => match target_ty {
                Type::Int => Ok(Type::Int),
                Type::I32 => {
                    if *val >= i32::MIN as i64 && *val <= i32::MAX as i64 {
                        Ok(Type::I32)
                    } else {
                        Err(MatcError::TypeError {
                            message: format!("Integer literal {} exceeds bounds for i32", val),
                        })
                    }
                }
                Type::I16 => {
                    if *val >= i16::MIN as i64 && *val <= i16::MAX as i64 {
                        Ok(Type::I16)
                    } else {
                        Err(MatcError::TypeError {
                            message: format!("Integer literal {} exceeds bounds for i16", val),
                        })
                    }
                }
                Type::I8 => {
                    if *val >= i8::MIN as i64 && *val <= i8::MAX as i64 {
                        Ok(Type::I8)
                    } else {
                        Err(MatcError::TypeError {
                            message: format!("Integer literal {} exceeds bounds for i8", val),
                        })
                    }
                }
                other => Err(MatcError::TypeError {
                    message: format!("Cannot check integer literal against type {:?}", other),
                }),
            },
            (Expression::FloatLiteral(_, _), Type::F32) => Ok(Type::F32),
            (Expression::FloatLiteral(_, _), Type::F64) => Ok(Type::F64),
            (Expression::Binary { left, right, .. }, target_ty) => {
                let left_ty = self.check_expr(left, target_ty, symbols)?;
                let right_ty = self.check_expr(right, target_ty, symbols)?;
                if left_ty != *target_ty || right_ty != *target_ty {
                    return Err(MatcError::TypeError {
                        message: format!(
                            "Type mismatch in binary expression: expected {:?}",
                            target_ty
                        ),
                    });
                }
                Ok(target_ty.clone())
            }
            (Expression::TupleLiteral(elements, _), Type::Tuple(target_types)) => {
                if elements.len() != target_types.len() {
                    return Err(MatcError::TypeError {
                        message: format!(
                            "Tuple length mismatch: expected {}, got {}",
                            target_types.len(),
                            elements.len()
                        ),
                    });
                }
                let mut checked_types = Vec::new();
                for (elem, target_elem_ty) in elements.iter().zip(target_types.iter()) {
                    checked_types.push(self.check_expr(elem, target_elem_ty, symbols)?);
                }
                Ok(Type::Tuple(checked_types))
            }
            (Expression::ArrayLiteral(elements, _), Type::Array(target_elem_ty, expected_len)) => {
                if elements.len() != *expected_len {
                    return Err(MatcError::TypeError {
                        message: format!(
                            "Array length mismatch: expected {}, got {}",
                            expected_len,
                            elements.len()
                        ),
                    });
                }
                for elem in elements {
                    self.check_expr(elem, target_elem_ty, symbols)?;
                }
                Ok(Type::Array(target_elem_ty.clone(), *expected_len))
            }
            (other_expr, target_ty) => {
                let synthesized = self.synthesize_expr(other_expr, symbols)?;
                if synthesized == *target_ty {
                    Ok(synthesized)
                } else {
                    Err(MatcError::TypeError {
                        message: format!(
                            "Type mismatch: expected {:?}, got {:?}",
                            target_ty, synthesized
                        ),
                    })
                }
            }
        }
    }
}
