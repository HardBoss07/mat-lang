use super::super::symbol_table::SymbolTable;
use super::TypeChecker;
use crate::ast::{Expression, Type};
use crate::error::Result;

impl TypeChecker {
    pub fn synthesize_expr(&self, expr: &Expression, symbols: &SymbolTable) -> Result<Type> {
        match expr {
            Expression::Identifier(name, span) => {
                let sym = symbols
                    .lookup(name)
                    .ok_or_else(|| self.undefined_variable(name, *span))?;
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
            Expression::ArrayLiteral(elements, span) => {
                if elements.is_empty() {
                    return Err(self.type_error(
                        "Cannot infer type of empty array literal without explicit annotation",
                        *span,
                    ));
                }
                let first_ty = self.synthesize_expr(&elements[0], symbols)?;
                for elem in &elements[1..] {
                    let elem_ty = self.synthesize_expr(elem, symbols)?;
                    if elem_ty != first_ty {
                        return Err(self.type_error(
                            format!(
                                "Mismatched types in array literal: expected {:?}, got {:?}",
                                first_ty, elem_ty
                            ),
                            elem.span(),
                        ));
                    }
                }
                Ok(Type::Array(Box::new(first_ty), elements.len()))
            }
            Expression::TupleAccess { expr, index, span } => {
                let expr_ty = self.synthesize_expr(expr, symbols)?;
                match expr_ty {
                    Type::Tuple(types) => types.get(*index).cloned().ok_or_else(|| {
                        self.type_error(
                            format!(
                                "Tuple index {} out of bounds for tuple length {}",
                                index,
                                types.len()
                            ),
                            *span,
                        )
                    }),
                    other => Err(self.type_error(
                        format!("Cannot access tuple index on non-tuple type {:?}", other),
                        expr.span(),
                    )),
                }
            }
            Expression::ArrayAccess { expr, index, span } => {
                let expr_ty = self.synthesize_expr(expr, symbols)?;
                let index_ty = self.synthesize_expr(index, symbols)?;
                if index_ty != Type::Int && index_ty != Type::I32 {
                    return Err(self.type_error(
                        format!("Array index must be an integer, got {:?}", index_ty),
                        index.span(),
                    ));
                }
                match expr_ty {
                    Type::Array(elem_ty, _) => Ok(*elem_ty),
                    other => {
                        Err(self
                            .type_error(format!("Cannot index non-array type {:?}", other), *span))
                    }
                }
            }
            Expression::Call {
                callee,
                arguments,
                span,
            } => {
                let fn_sym = symbols
                    .lookup_function(callee)
                    .ok_or_else(|| self.undefined_function(callee, *span))?
                    .clone();

                if callee != "println" && arguments.len() != fn_sym.param_types.len() {
                    return Err(self.type_error(
                        format!(
                            "Function '{}' expects {} arguments, got {}",
                            callee,
                            fn_sym.param_types.len(),
                            arguments.len()
                        ),
                        *span,
                    ));
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
            Expression::Ok(_, span) | Expression::Err(_, span) => Err(self.type_error(
                "Ok(...) and Err(...) constructors require type context",
                *span,
            )),
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
            (Expression::IntLiteral(val, span), target_ty) => match target_ty {
                Type::Int => Ok(Type::Int),
                Type::I32 => {
                    if *val >= i32::MIN as i64 && *val <= i32::MAX as i64 {
                        Ok(Type::I32)
                    } else {
                        Err(self.type_error(
                            format!("Integer literal {} exceeds bounds for i32", val),
                            *span,
                        ))
                    }
                }
                Type::I16 => {
                    if *val >= i16::MIN as i64 && *val <= i16::MAX as i64 {
                        Ok(Type::I16)
                    } else {
                        Err(self.type_error(
                            format!("Integer literal {} exceeds bounds for i16", val),
                            *span,
                        ))
                    }
                }
                Type::I8 => {
                    if *val >= i8::MIN as i64 && *val <= i8::MAX as i64 {
                        Ok(Type::I8)
                    } else {
                        Err(self.type_error(
                            format!("Integer literal {} exceeds bounds for i8", val),
                            *span,
                        ))
                    }
                }
                other => Err(self.type_error(
                    format!("Cannot check integer literal against type {:?}", other),
                    *span,
                )),
            },
            (Expression::FloatLiteral(_, _), Type::F32) => Ok(Type::F32),
            (Expression::FloatLiteral(_, _), Type::F64) => Ok(Type::F64),
            (
                Expression::Binary {
                    op, left, right, ..
                },
                target_ty,
            ) => {
                let synthesized_ty = self.synthesize_binary_expr(op, left, right, symbols)?;
                if synthesized_ty == *target_ty {
                    Ok(synthesized_ty)
                } else {
                    Err(self.type_error(
                        format!(
                            "Type mismatch in binary expression: expected {:?}, got {:?}",
                            target_ty, synthesized_ty
                        ),
                        expr.span(),
                    ))
                }
            }
            (Expression::TupleLiteral(elements, span), Type::Tuple(target_types)) => {
                if elements.len() != target_types.len() {
                    return Err(self.type_error(
                        format!(
                            "Tuple length mismatch: expected {}, got {}",
                            target_types.len(),
                            elements.len()
                        ),
                        *span,
                    ));
                }
                let mut checked_types = Vec::new();
                for (elem, target_elem_ty) in elements.iter().zip(target_types.iter()) {
                    checked_types.push(self.check_expr(elem, target_elem_ty, symbols)?);
                }
                Ok(Type::Tuple(checked_types))
            }
            (
                Expression::ArrayLiteral(elements, span),
                Type::Array(target_elem_ty, expected_len),
            ) => {
                if elements.len() != *expected_len {
                    return Err(self.type_error(
                        format!(
                            "Array length mismatch: expected {}, got {}",
                            expected_len,
                            elements.len()
                        ),
                        *span,
                    ));
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
                    Err(self.type_error(
                        format!(
                            "Type mismatch: expected {:?}, got {:?}",
                            target_ty, synthesized
                        ),
                        other_expr.span(),
                    ))
                }
            }
        }
    }
}
