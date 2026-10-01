use crate::ast::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub name: String,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
    functions: HashMap<String, FunctionSymbol>,
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
        global_scope.insert(
            "print".to_string(),
            Symbol {
                name: "print".to_string(),
                ty: Type::Void,
                is_mutable: false,
            },
        );

        let mut functions = HashMap::new();
        functions.insert(
            "println".to_string(),
            FunctionSymbol {
                name: "println".to_string(),
                param_types: vec![],
                return_type: Type::Void,
            },
        );
        functions.insert(
            "print".to_string(),
            FunctionSymbol {
                name: "print".to_string(),
                param_types: vec![],
                return_type: Type::Void,
            },
        );

        // System C runtime helper bindings
        functions.insert(
            "_mat_rt_read_file".to_string(),
            FunctionSymbol {
                name: "_mat_rt_read_file".to_string(),
                param_types: vec![Type::String],
                return_type: Type::String,
            },
        );
        functions.insert(
            "_mat_rt_write_file".to_string(),
            FunctionSymbol {
                name: "_mat_rt_write_file".to_string(),
                param_types: vec![Type::String, Type::String],
                return_type: Type::Bool,
            },
        );
        functions.insert(
            "_mat_rt_print_str".to_string(),
            FunctionSymbol {
                name: "_mat_rt_print_str".to_string(),
                param_types: vec![Type::String],
                return_type: Type::Void,
            },
        );

        Self {
            scopes: vec![global_scope],
            functions,
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
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

    pub fn insert_function(&mut self, name: String, param_types: Vec<Type>, return_type: Type) {
        self.functions.insert(
            name.clone(),
            FunctionSymbol {
                name,
                param_types,
                return_type,
            },
        );
    }

    pub fn lookup_function(&self, name: &str) -> Option<&FunctionSymbol> {
        self.functions.get(name)
    }
}
