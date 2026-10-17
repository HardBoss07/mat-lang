use crate::ast::{EnumVariantPayload, Type};
use crate::error::{MatcError, Result};
use std::cell::RefCell;
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

#[derive(Debug, Clone)]
pub struct StructFieldSymbol {
    pub is_pub: bool,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct StructSymbol {
    pub name: String,
    pub fields: Vec<StructFieldSymbol>,
}

#[derive(Debug, Clone)]
pub struct EnumVariantSymbol {
    pub name: String,
    pub payload: EnumVariantPayload,
}

#[derive(Debug, Clone)]
pub struct EnumSymbol {
    pub name: String,
    pub variants: Vec<EnumVariantSymbol>,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    scopes: RefCell<Vec<HashMap<String, Symbol>>>,
    functions: HashMap<String, FunctionSymbol>,
    structs: HashMap<String, StructSymbol>,
    enums: HashMap<String, EnumSymbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut table = Self {
            scopes: RefCell::new(vec![HashMap::new()]),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
        };
        table.register_runtime_intrinsics();
        table
    }

    pub fn register_runtime_intrinsics(&mut self) {
        // Console I/O Intrinsics
        self.insert_function("_mat_rt_print_str".into(), vec![Type::String], Type::Void);
        self.insert_function("_mat_rt_println_str".into(), vec![Type::String], Type::Void);
        self.insert_function("_mat_rt_read_line".into(), vec![], Type::String);

        // File I/O Intrinsics
        self.insert_function(
            "_mat_rt_file_open".into(),
            vec![Type::String, Type::String],
            Type::Int,
        );
        self.insert_function(
            "_mat_rt_file_read_line".into(),
            vec![Type::Int],
            Type::String,
        );
        self.insert_function(
            "_mat_rt_file_read_to_string".into(),
            vec![Type::Int],
            Type::String,
        );
        self.insert_function(
            "_mat_rt_file_write".into(),
            vec![Type::Int, Type::String],
            Type::Int,
        );
        self.insert_function("_mat_rt_file_flush".into(), vec![Type::Int], Type::Bool);
        self.insert_function(
            "_mat_rt_file_seek".into(),
            vec![Type::Int, Type::Int, Type::Int],
            Type::Int,
        );
        self.insert_function("_mat_rt_file_close".into(), vec![Type::Int], Type::Bool);

        // Math Intrinsics
        self.insert_function("_mat_rt_sin".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_cos".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_tan".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_asin".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_acos".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_atan".into(), vec![Type::F64], Type::F64);
        self.insert_function(
            "_mat_rt_atan2".into(),
            vec![Type::F64, Type::F64],
            Type::F64,
        );
        self.insert_function("_mat_rt_sqrt".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_pow".into(), vec![Type::F64, Type::F64], Type::F64);
        self.insert_function("_mat_rt_abs_f64".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_abs_int".into(), vec![Type::Int], Type::Int);
        self.insert_function("_mat_rt_floor".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_ceil".into(), vec![Type::F64], Type::F64);
        self.insert_function("_mat_rt_round".into(), vec![Type::F64], Type::F64);
    }

    pub fn push_scope(&self) {
        self.scopes.borrow_mut().push(HashMap::new());
    }

    pub fn pop_scope(&self) {
        let mut scopes = self.scopes.borrow_mut();
        if scopes.len() > 1 {
            scopes.pop();
        }
    }

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

    pub fn update_type(&self, name: &str, new_ty: Type) -> bool {
        for scope in self.scopes.borrow_mut().iter_mut().rev() {
            if let Some(sym) = scope.get_mut(name) {
                sym.ty = new_ty;
                return true;
            }
        }
        false
    }

    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.borrow().iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym.clone());
            }
        }
        None
    }

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
        if let Some(f) = self.functions.get(name) {
            return Some(f);
        }
        let short_name = name.split("::").last().unwrap_or(name);
        let intrinsic_name = format!("_mat_rt_{}", short_name);
        if let Some(f) = self.functions.get(&intrinsic_name) {
            return Some(f);
        }

        if name.contains("::") {
            let suffix = format!("::{}", name);
            for (key, f) in &self.functions {
                if key.ends_with(&suffix) {
                    return Some(f);
                }
            }
            if let Some(short_name) = name.split("::").last() {
                if let Some(f) = self.functions.get(short_name) {
                    return Some(f);
                }
                let short_suffix = format!("::{}", short_name);
                for (key, f) in &self.functions {
                    if key.ends_with(&short_suffix) {
                        return Some(f);
                    }
                }
            }
        } else {
            let suffix = format!("::{}", name);
            for (key, f) in &self.functions {
                if key == short_name || key.ends_with(&suffix) {
                    return Some(f);
                }
            }
        }
        None
    }

    pub fn insert_struct(&mut self, name: String, fields: Vec<StructFieldSymbol>) {
        self.structs
            .insert(name.clone(), StructSymbol { name, fields });
    }

    pub fn lookup_struct(&self, name: &str) -> Option<&StructSymbol> {
        if let Some(s) = self.structs.get(name) {
            return Some(s);
        }
        if name.contains("::") {
            let suffix = format!("::{}", name);
            for (key, s) in &self.structs {
                if key.ends_with(&suffix) {
                    return Some(s);
                }
            }
            if let Some(short_name) = name.split("::").last() {
                if let Some(s) = self.structs.get(short_name) {
                    return Some(s);
                }
                let short_suffix = format!("::{}", short_name);
                for (key, s) in &self.structs {
                    if key.ends_with(&short_suffix) {
                        return Some(s);
                    }
                }
            }
        } else {
            let suffix = format!("::{}", name);
            for (key, s) in &self.structs {
                if key.ends_with(&suffix) {
                    return Some(s);
                }
            }
        }
        None
    }

    pub fn insert_enum(&mut self, name: String, variants: Vec<EnumVariantSymbol>) {
        self.enums
            .insert(name.clone(), EnumSymbol { name, variants });
    }

    pub fn lookup_enum(&self, name: &str) -> Option<&EnumSymbol> {
        if let Some(e) = self.enums.get(name) {
            return Some(e);
        }
        if name.contains("::") {
            let suffix = format!("::{}", name);
            for (key, e) in &self.enums {
                if key.ends_with(&suffix) {
                    return Some(e);
                }
            }
            if let Some(short_name) = name.split("::").last() {
                if let Some(e) = self.enums.get(short_name) {
                    return Some(e);
                }
                let short_suffix = format!("::{}", short_name);
                for (key, e) in &self.enums {
                    if key.ends_with(&short_suffix) {
                        return Some(e);
                    }
                }
            }
        } else {
            let suffix = format!("::{}", name);
            for (key, e) in &self.enums {
                if key.ends_with(&suffix) {
                    return Some(e);
                }
            }
        }
        None
    }
}

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
