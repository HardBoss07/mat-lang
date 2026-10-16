//! AST name qualification and prefixing for imported modules.
//!
//! This module rewrites symbols, calls, and references in imported module ASTs
//! so that identifiers are qualified with their module path prefix.

use crate::ast::{Expression, Item, Statement};

/// Prefixes top-level declaration names and internal references within items with a module prefix.
///
/// # Arguments
///
/// * `items` - The mutable slice of items to prefix.
/// * `prefix` - The module namespace prefix to apply.
pub fn prefix_module_items(items: &mut [Item], prefix: &str) {
    for item in items {
        match item {
            Item::Function(func) => {
                if !func.name.contains("::") {
                    func.name = format!("{}::{}", prefix, func.name);
                }
                for statement in &mut func.body {
                    prefix_statement(statement, prefix);
                }
            }
            Item::Struct(struct_decl) => {
                if !struct_decl.name.contains("::") {
                    struct_decl.name = format!("{}::{}", prefix, struct_decl.name);
                }
            }
            Item::Enum(enum_decl) => {
                if !enum_decl.name.contains("::") {
                    enum_decl.name = format!("{}::{}", prefix, enum_decl.name);
                }
            }
            Item::GlobalLet(global_let) => {
                if !global_let.name.contains("::") {
                    global_let.name = format!("{}::{}", prefix, global_let.name);
                }
                prefix_expression(&mut global_let.value, prefix);
            }
            Item::Import(_) => {}
        }
    }
}

/// Recursively prefixes references within statements.
pub fn prefix_statement(statement: &mut Statement, prefix: &str) {
    match statement {
        Statement::Let { value, .. }
        | Statement::Assignment { value, .. }
        | Statement::CompoundAssignment { value, .. } => prefix_expression(value, prefix),
        Statement::Increment { .. } | Statement::Decrement { .. } => {}
        Statement::Loop { body, .. } => prefix_statement_block(body, prefix),
        Statement::While {
            condition, body, ..
        } => {
            prefix_expression(condition, prefix);
            prefix_statement_block(body, prefix);
        }
        Statement::ForI {
            init,
            condition,
            step,
            body,
            ..
        } => {
            prefix_statement(init, prefix);
            prefix_expression(condition, prefix);
            prefix_statement(step, prefix);
            prefix_statement_block(body, prefix);
        }
        Statement::ForIn { iterable, body, .. } => {
            prefix_expression(iterable, prefix);
            prefix_statement_block(body, prefix);
        }
        Statement::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            prefix_expression(condition, prefix);
            prefix_statement_block(then_branch, prefix);
            if let Some(else_stmts) = else_branch {
                prefix_statement_block(else_stmts, prefix);
            }
        }
        Statement::Match { expr, arms, .. } => {
            prefix_expression(expr, prefix);
            for arm in arms {
                prefix_statement_block(&mut arm.body, prefix);
            }
        }
        Statement::Return(opt_expr, _) => {
            if let Some(expr) = opt_expr {
                prefix_expression(expr, prefix);
            }
        }
        Statement::Break(_) | Statement::Continue(_) => {}
        Statement::Expression(expr) => prefix_expression(expr, prefix),
    }
}

/// Helper to prefix an entire sequence of statements.
fn prefix_statement_block(statements: &mut [Statement], prefix: &str) {
    for statement in statements {
        prefix_statement(statement, prefix);
    }
}

/// Recursively prefixes callee names and nested subexpressions with the module prefix.
pub fn prefix_expression(expr: &mut Expression, prefix: &str) {
    match expr {
        Expression::Call {
            callee, arguments, ..
        } => {
            prefix_call_callee(callee, prefix);
            for argument in arguments {
                prefix_expression(argument, prefix);
            }
        }
        Expression::Binary { left, right, .. } => {
            prefix_expression(left, prefix);
            prefix_expression(right, prefix);
        }
        Expression::TupleLiteral(elements, _)
        | Expression::ArrayLiteral(elements, _)
        | Expression::VectorLiteral(elements, _) => {
            for element in elements {
                prefix_expression(element, prefix);
            }
        }
        Expression::StructInit { fields, .. } => {
            for (_, field_expr) in fields {
                prefix_expression(field_expr, prefix);
            }
        }
        Expression::FieldAccess {
            expr: base_expr, ..
        } => {
            prefix_expression(base_expr, prefix);
        }
        Expression::MethodCall { receiver, args, .. } => {
            prefix_expression(receiver, prefix);
            for arg in args {
                prefix_expression(arg, prefix);
            }
        }
        Expression::InterpolatedString(parts, _) => {
            for (part, _) in parts {
                prefix_expression(part, prefix);
            }
        }
        Expression::Ok(inner_expr, _) | Expression::Err(inner_expr, _) => {
            prefix_expression(inner_expr, prefix);
        }
        Expression::TupleAccess {
            expr: base_expr, ..
        } => {
            prefix_expression(base_expr, prefix);
        }
        Expression::ArrayAccess {
            expr: base_expr,
            index,
            ..
        } => {
            prefix_expression(base_expr, prefix);
            prefix_expression(index, prefix);
        }
        _ => {}
    }
}

/// Helper to prefix non-built-in callee identifiers with the module prefix.
fn prefix_call_callee(callee: &mut String, prefix: &str) {
    if !callee.contains("::")
        && callee != "println"
        && callee != "print"
        && !callee.starts_with("_mat_rt_")
    {
        *callee = format!("{}::{}", prefix, callee);
    }
}
