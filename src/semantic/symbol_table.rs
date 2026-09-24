use crate::ast::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
    pub is_mutable: bool,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut global_scope = HashMap::new();
        global_scope.insert(
            "println".to_string(),
            Symbol {
                name: "println".to_string(),
                ty: Type::Void,
                is_mutable: false,
            },
        );

        Self {
            scopes: vec![global_scope],
        }
    }

    pub fn insert(&mut self, name: String, ty: Type, is_mutable: bool) {
        if let Some(scope) = self.scopes.last_mut() {
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

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}
