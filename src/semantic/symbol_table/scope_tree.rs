//! Lexical scope tree and local variable symbol storage.
//!
//! Encapsulates stacked scope frames, shadowing rules, and resolution across nested scopes.

use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::Type;
use crate::error::{MatcError, Result};

/// Representation of a local variable symbol in a lexical scope frame.
#[derive(Debug, Clone)]
pub struct Symbol {
    /// Variable identifier.
    pub name: String,
    /// Inferred or declared type.
    pub ty: Type,
    /// Mutation permission flag.
    pub is_mutable: bool,
}

/// A stack-based lexical scope manager for local symbols.
#[derive(Debug, Clone)]
pub struct ScopeTree {
    scopes: RefCell<Vec<HashMap<String, Symbol>>>,
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeTree {
    /// Constructs a scope tree initialized with an empty global scope.
    pub fn new() -> Self {
        Self {
            scopes: RefCell::new(vec![HashMap::new()]),
        }
    }

    /// Pushes a new nested lexical scope level.
    pub fn push_scope(&self) {
        self.scopes.borrow_mut().push(HashMap::new());
    }

    /// Pops the innermost scope level if more than the root scope remains.
    pub fn pop_scope(&self) {
        let mut scopes = self.scopes.borrow_mut();
        if scopes.len() > 1 {
            scopes.pop();
        }
    }

    /// Inserts a variable symbol into the current innermost scope.
    pub fn insert(&self, name: String, ty: Type, is_mutable: bool) {
        if let Some(scope) = self.scopes.borrow_mut().last_mut() {
            scope.insert(
                name.clone(),
                Symbol {
                    name,
                    ty,
                    is_mutable,
                },
            );
        }
    }

    /// Updates the type of an existing symbol in the innermost scope containing it.
    pub fn update_type(&self, name: &str, new_ty: Type) -> bool {
        for scope in self.scopes.borrow_mut().iter_mut().rev() {
            if let Some(sym) = scope.get_mut(name) {
                sym.ty = new_ty;
                return true;
            }
        }
        false
    }

    /// Looks up a variable symbol, searching from innermost scope to outermost scope.
    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.borrow().iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym.clone());
            }
        }
        None
    }

    /// Verifies that no variables in the active scope contain an unresolved `Type::Infer`.
    pub fn verify_no_unresolved_infer_types<F>(&self, mut make_err: F) -> Result<()>
    where
        F: FnMut(&str) -> MatcError,
    {
        if let Some(scope) = self.scopes.borrow().last() {
            for (name, sym) in scope {
                if is_infer_type(&sym.ty) {
                    return Err(make_err(name));
                }
            }
        }
        Ok(())
    }
}

/// Recursively checks whether a type contains an unresolved `Type::Infer`.
fn is_infer_type(ty: &Type) -> bool {
    match ty {
        Type::Infer => true,
        Type::Vector(elem) => is_infer_type(elem),
        Type::Array(elem, _) => is_infer_type(elem),
        Type::Tuple(elems) => elems.iter().any(is_infer_type),
        Type::Result(ok, err) => is_infer_type(ok) || is_infer_type(err),
        _ => false,
    }
}
