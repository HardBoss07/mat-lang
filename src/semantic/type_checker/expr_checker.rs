use super::super::symbol_table::SymbolTable;
use super::TypeChecker;
use crate::ast::{Expression, Type};
use crate::error::Result;

pub fn types_compatible(t1: &Type, t2: &Type) -> bool {
    if t1 == t2 {
        return true;
    }
    match (t1, t2) {
        (Type::Custom(a) | Type::Struct(a), Type::Custom(b) | Type::Struct(b)) => {
            let short_a = a.split("::").last().unwrap_or(a);
            let short_b = b.split("::").last().unwrap_or(b);
            short_a == short_b
        }
        (Type::Result(a_ok, a_err), Type::Result(b_ok, b_err)) => {
            types_compatible(a_ok, b_ok) && types_compatible(a_err, b_err)
        }
        (Type::Vector(a), Type::Vector(b)) => types_compatible(a, b),
        (Type::Array(a, len_a), Type::Array(b, len_b)) => len_a == len_b && types_compatible(a, b),
        (Type::Tuple(elems_a), Type::Tuple(elems_b)) => {
            elems_a.len() == elems_b.len()
                && elems_a
                    .iter()
                    .zip(elems_b.iter())
                    .all(|(ea, eb)| types_compatible(ea, eb))
        }
        _ => false,
    }
}

impl TypeChecker {
    pub fn synthesize_expr(&self, expr: &Expression, symbols: &SymbolTable) -> Result<Type> {
        match expr {
            Expression::Identifier(name, span) => {
                if let Some(sym) = symbols.lookup(name) {
                    Ok(sym.ty.clone())
                } else if name.contains("::") {
                    let parts: Vec<&str> = name.split("::").collect();
                    let enum_name = parts[..parts.len() - 1].join("::");
                    let variant_name = parts.last().unwrap();

                    if let Some(enum_def) = symbols.lookup_enum(&enum_name) {
                        if let Some(variant) =
                            enum_def.variants.iter().find(|v| v.name == *variant_name)
                        {
                            if matches!(variant.payload, crate::ast::EnumVariantPayload::Unit) {
                                return Ok(Type::Custom(enum_name));
                            }
                        } else {
                            return Err(self.type_error(
                                format!(
                                    "Enum variant '{}' not found on enum '{}'",
                                    variant_name, enum_name
                                ),
                                *span,
                            ));
                        }
                    }
                    Err(self.undefined_variable(name, *span))
                } else {
                    Err(self.undefined_variable(name, *span))
                }
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
                    if !types_compatible(&elem_ty, &first_ty) {
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
            Expression::VectorLiteral(elements, span) => {
                if elements.is_empty() {
                    return Err(self.type_error(
                        "Cannot infer type of empty vector literal without explicit annotation",
                        *span,
                    ));
                }
                let first_ty = self.synthesize_expr(&elements[0], symbols)?;
                for elem in &elements[1..] {
                    let elem_ty = self.synthesize_expr(elem, symbols)?;
                    if !types_compatible(&elem_ty, &first_ty) {
                        return Err(self.type_error(
                            format!(
                                "Mismatched types in vector literal: expected {:?}, got {:?}",
                                first_ty, elem_ty
                            ),
                            elem.span(),
                        ));
                    }
                }
                Ok(Type::Vector(Box::new(first_ty)))
            }
            Expression::StructInit { name, fields, span } => {
                if name.contains("::") {
                    let parts: Vec<&str> = name.split("::").collect();
                    let enum_name = parts[..parts.len() - 1].join("::");
                    let variant_name = parts.last().unwrap();

                    if let Some(enum_def) = symbols.lookup_enum(&enum_name).cloned() {
                        if let Some(variant) =
                            enum_def.variants.iter().find(|v| v.name == *variant_name)
                        {
                            if let crate::ast::EnumVariantPayload::Struct(expected_fields) =
                                &variant.payload
                            {
                                if fields.len() != expected_fields.len() {
                                    return Err(self.type_error(
                                        format!(
                                            "Enum variant '{}::{}' expects {} fields, got {}",
                                            enum_name,
                                            variant_name,
                                            expected_fields.len(),
                                            fields.len()
                                        ),
                                        *span,
                                    ));
                                }

                                for (field_name, field_expr) in fields {
                                    let expected_field_ty = expected_fields
                                        .iter()
                                        .find(|f| f.name == *field_name)
                                        .map(|f| &f.ty)
                                        .ok_or_else(|| {
                                            self.type_error(
                                                format!(
                                                    "Unknown field '{}' on enum variant '{}::{}'",
                                                    field_name, enum_name, variant_name
                                                ),
                                                field_expr.span(),
                                            )
                                        })?;
                                    self.check_expr(field_expr, expected_field_ty, symbols)?;
                                }

                                return Ok(Type::Custom(enum_name));
                            }
                        }
                    }
                }

                let struct_def = symbols
                    .lookup_struct(name)
                    .ok_or_else(|| self.type_error(format!("Undefined struct '{}'", name), *span))?
                    .clone();

                if fields.len() != struct_def.fields.len() {
                    return Err(self.type_error(
                        format!(
                            "Struct '{}' expects {} fields, got {}",
                            name,
                            struct_def.fields.len(),
                            fields.len()
                        ),
                        *span,
                    ));
                }

                for (field_name, field_expr) in fields {
                    let expected_field_ty = struct_def
                        .fields
                        .iter()
                        .find(|f| f.name == *field_name)
                        .map(|f| &f.ty)
                        .ok_or_else(|| {
                            self.type_error(
                                format!("Unknown field '{}' on struct '{}'", field_name, name),
                                field_expr.span(),
                            )
                        })?;
                    self.check_expr(field_expr, expected_field_ty, symbols)?;
                }

                Ok(Type::Struct(name.clone()))
            }
            Expression::EnumInit {
                enum_name,
                variant_name,
                payload,
                span,
            } => {
                let e_name = match enum_name {
                    Some(name) => name.clone(),
                    None => variant_name.clone(),
                };

                let enum_def = symbols
                    .lookup_enum(&e_name)
                    .ok_or_else(|| self.type_error(format!("Undefined enum '{}'", e_name), *span))?
                    .clone();

                let _variant = enum_def
                    .variants
                    .iter()
                    .find(|v| v.name == *variant_name)
                    .ok_or_else(|| {
                        self.type_error(
                            format!("Enum '{}' has no variant '{}'", e_name, variant_name),
                            *span,
                        )
                    })?;

                if let Some(payload_expr) = payload {
                    self.synthesize_expr(payload_expr, symbols)?;
                }

                Ok(Type::Custom(e_name))
            }
            Expression::FieldAccess { expr, field, span } => {
                let expr_ty = self.synthesize_expr(expr, symbols)?;
                let struct_name = match &expr_ty {
                    Type::Struct(name) => name,
                    Type::Custom(name) => name,
                    other => {
                        return Err(self.type_error(
                            format!(
                                "Cannot access field '{}' on non-struct type {:?}",
                                field, other
                            ),
                            expr.span(),
                        ));
                    }
                };

                let struct_def = symbols.lookup_struct(struct_name).ok_or_else(|| {
                    self.type_error(format!("Undefined struct '{}'", struct_name), *span)
                })?;

                let field_symbol = struct_def
                    .fields
                    .iter()
                    .find(|f| f.name == *field)
                    .ok_or_else(|| {
                        self.type_error(
                            format!("Struct '{}' has no field '{}'", struct_name, field),
                            *span,
                        )
                    })?;

                if struct_name.contains("::") && !field_symbol.is_pub {
                    return Err(self.type_error(
                        format!(
                            "Undefined private field '{}' on struct '{}'",
                            field, struct_name
                        ),
                        *span,
                    ));
                }

                Ok(field_symbol.ty.clone())
            }
            Expression::MethodCall {
                receiver,
                method,
                args,
                span,
            } => {
                let receiver_ty = self.synthesize_expr(receiver, symbols)?;
                self.check_method_call(&receiver_ty, method, args, *span, symbols)
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
                    Type::Vector(elem_ty) => Ok(*elem_ty),
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
                if callee == "Vec::new" {
                    return Err(
                        self.type_error("Vec::new() requires type annotation context", *span)
                    );
                }

                if callee == "println" || callee == "print" {
                    for arg in arguments {
                        self.synthesize_expr(arg, symbols)?;
                    }
                    return Ok(Type::Void);
                }

                if let Some(fn_sym) = symbols.lookup_function(callee).cloned() {
                    if callee != "println"
                        && !callee.starts_with("_mat_rt_")
                        && arguments.len() != fn_sym.param_types.len()
                    {
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
                    return Ok(fn_sym.return_type);
                }

                if callee.starts_with("_mat_rt_") {
                    let mut param_types = Vec::new();
                    for arg in arguments {
                        param_types.push(self.synthesize_expr(arg, symbols)?);
                    }
                    let return_type =
                        if callee.contains("read_line") || callee.contains("read_to_string") {
                            Type::String
                        } else if callee.contains("open")
                            || callee.contains("write")
                            || callee.contains("seek")
                            || callee.contains("abs_int")
                        {
                            Type::Int
                        } else if callee.contains("flush") || callee.contains("close") {
                            Type::Bool
                        } else if callee.contains("print") {
                            Type::Void
                        } else {
                            Type::F64
                        };

                    if callee != "println" {
                        for arg in arguments {
                            self.synthesize_expr(arg, symbols)?;
                        }
                    }
                    return Ok(return_type);
                }

                if callee.contains("::") {
                    let parts: Vec<&str> = callee.split("::").collect();
                    let enum_name = parts[..parts.len() - 1].join("::");
                    let variant_name = parts.last().unwrap();

                    if let Some(enum_def) = symbols.lookup_enum(&enum_name).cloned() {
                        if let Some(variant) =
                            enum_def.variants.iter().find(|v| v.name == *variant_name)
                        {
                            match &variant.payload {
                                crate::ast::EnumVariantPayload::Tuple(expected_types) => {
                                    if arguments.len() != expected_types.len() {
                                        return Err(self.type_error(
                                            format!(
                                                "Enum variant '{}::{}' expects {} arguments, got {}",
                                                enum_name,
                                                variant_name,
                                                expected_types.len(),
                                                arguments.len()
                                            ),
                                            *span,
                                        ));
                                    }
                                    for (arg, expected_ty) in
                                        arguments.iter().zip(expected_types.iter())
                                    {
                                        self.check_expr(arg, expected_ty, symbols)?;
                                    }
                                }
                                crate::ast::EnumVariantPayload::Unit => {
                                    if !arguments.is_empty() {
                                        return Err(self.type_error(
                                            format!(
                                                "Unit enum variant '{}::{}' takes 0 arguments",
                                                enum_name, variant_name
                                            ),
                                            *span,
                                        ));
                                    }
                                }
                                crate::ast::EnumVariantPayload::Struct(_) => {}
                            }
                            return Ok(Type::Custom(enum_name));
                        }
                    }
                }

                Err(self.undefined_function(callee, *span))
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
                if types_compatible(&synthesized_ty, target_ty) {
                    Ok(target_ty.clone())
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
            (Expression::VectorLiteral(elements, _), Type::Vector(target_elem_ty)) => {
                for elem in elements {
                    self.check_expr(elem, target_elem_ty, symbols)?;
                }
                Ok(Type::Vector(target_elem_ty.clone()))
            }
            (
                Expression::Call {
                    callee,
                    arguments,
                    span,
                },
                Type::Vector(target_elem_ty),
            ) if callee == "Vec::new" => {
                if !arguments.is_empty() {
                    return Err(self.type_error("Vec::new() takes 0 arguments", *span));
                }
                Ok(Type::Vector(target_elem_ty.clone()))
            }
            (
                Expression::Call {
                    callee,
                    arguments: _,
                    span: _,
                },
                target_ty @ (Type::Custom(target_name) | Type::Struct(target_name)),
            ) => {
                let target_short = target_name.split("::").last().unwrap_or(target_name);
                if callee.contains("::") {
                    let parts: Vec<&str> = callee.split("::").collect();
                    let enum_name = parts[..parts.len() - 1].join("::");
                    let enum_short = enum_name.split("::").last().unwrap_or(&enum_name);

                    if enum_short == target_short {
                        self.synthesize_expr(expr, symbols)?;
                        return Ok(target_ty.clone());
                    }
                }
                let synthesized = self.synthesize_expr(expr, symbols)?;
                if types_compatible(&synthesized, target_ty) {
                    Ok(target_ty.clone())
                } else {
                    Err(self.type_error(
                        format!(
                            "Type mismatch: expected {:?}, got {:?}",
                            target_ty, synthesized
                        ),
                        expr.span(),
                    ))
                }
            }
            (
                Expression::StructInit { name, .. },
                target_ty @ (Type::Struct(target_name) | Type::Custom(target_name)),
            ) => {
                let synthesized = self.synthesize_expr(expr, symbols)?;
                if types_compatible(&synthesized, target_ty) {
                    Ok(target_ty.clone())
                } else {
                    Err(self.type_error(
                        format!(
                            "Type mismatch in struct initialization: expected {}, got {}",
                            target_name, name
                        ),
                        expr.span(),
                    ))
                }
            }
            (
                Expression::EnumInit {
                    enum_name,
                    variant_name: _,
                    payload,
                    span,
                },
                target_ty @ (Type::Custom(target_name) | Type::Struct(target_name)),
            ) => {
                let target_short = target_name.split("::").last().unwrap_or(target_name);
                let e_name = enum_name
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(target_short);
                let e_short = e_name.split("::").last().unwrap_or(e_name);
                if e_short != target_short {
                    return Err(self.type_error(
                        format!(
                            "Type mismatch in enum initialization: expected {}, got {}",
                            target_name, e_name
                        ),
                        *span,
                    ));
                }
                if let Some(payload_expr) = payload {
                    self.synthesize_expr(payload_expr, symbols)?;
                }
                Ok(target_ty.clone())
            }
            (other_expr, target_ty) => {
                let synthesized = self.synthesize_expr(other_expr, symbols)?;
                if types_compatible(&synthesized, target_ty) {
                    Ok(target_ty.clone())
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

    pub fn check_method_call(
        &self,
        receiver_ty: &Type,
        method: &str,
        args: &[Expression],
        span: crate::ast::Span,
        symbols: &SymbolTable,
    ) -> Result<Type> {
        match (receiver_ty, method) {
            (Type::Vector(elem_ty), "push") => {
                if args.is_empty() {
                    return Err(self.type_error("push requires at least 1 argument", span));
                }
                for arg in args {
                    self.check_expr(arg, elem_ty, symbols)?;
                }
                Ok(Type::Void)
            }
            (Type::Vector(elem_ty), "pop") => {
                if !args.is_empty() {
                    return Err(self.type_error("pop takes 0 arguments", span));
                }
                Ok((**elem_ty).clone())
            }
            (Type::Vector(elem_ty), "get") => {
                if args.len() != 1 {
                    return Err(self.type_error("get takes exactly 1 argument", span));
                }
                let index_ty = self.synthesize_expr(&args[0], symbols)?;
                if index_ty != Type::Int && index_ty != Type::I32 {
                    return Err(self.type_error("get index must be integer", args[0].span()));
                }
                Ok((**elem_ty).clone())
            }
            (Type::Vector(_), "len") | (Type::Array(_, _), "len") | (Type::Tuple(_), "len") => {
                if !args.is_empty() {
                    return Err(self.type_error("len takes 0 arguments", span));
                }
                Ok(Type::Int)
            }
            _ => Err(self.type_error(
                format!("Method '{}' not found on type {:?}", method, receiver_ty),
                span,
            )),
        }
    }
}
