use crate::ast::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut global_scope = HashMap::new();
        // Register built-in functions
        global_scope.insert(
            "println".to_string(),
            Symbol {
                name: "println".to_string(),
                ty: Type::Void,
            },
        );

        Self {
            scopes: vec![global_scope],
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
