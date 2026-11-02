//! Semantic analysis symbol table and environment scopes.
//!
//! Stores variable bindings, function signatures, struct layouts, and enum variant definitions
//! with lexical scoping support and qualified path lookup.

pub mod scope_tree;

#[allow(unused_imports)]
pub use scope_tree::*;

use crate::ast::{EnumVariantPayload, Type};
use crate::error::{MatcError, Result};
use std::collections::HashMap;

/// Representation of a function declaration symbol.
#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    /// Function name or qualified identifier.
    pub name: String,
    /// Parameter types in declaration order.
    pub param_types: Vec<Type>,
    /// Function return type.
    pub return_type: Type,
}

/// Representation of a single struct field.
#[derive(Debug, Clone)]
pub struct StructFieldSymbol {
    /// Field visibility modifier.
    pub is_pub: bool,
    /// Field name.
    pub name: String,
    /// Field type.
    pub ty: Type,
}

/// Representation of a struct declaration symbol.
#[derive(Debug, Clone)]
pub struct StructSymbol {
    /// Struct name.
    pub name: String,
    /// Ordered sequence of fields.
    pub fields: Vec<StructFieldSymbol>,
}

/// Representation of an enum variant with unit, tuple, or struct payload.
#[derive(Debug, Clone)]
pub struct EnumVariantSymbol {
    /// Variant name.
    pub name: String,
    /// Variant payload structure.
    pub payload: EnumVariantPayload,
}

/// Representation of an enum declaration symbol.
#[derive(Debug, Clone)]
pub struct EnumSymbol {
    /// Enum name.
    pub name: String,
    /// List of variants.
    pub variants: Vec<EnumVariantSymbol>,
}

/// Lexically scoped symbol table for semantic analysis and code generation.
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    scope_tree: ScopeTree,
    functions: HashMap<String, FunctionSymbol>,
    structs: HashMap<String, StructSymbol>,
    enums: HashMap<String, EnumSymbol>,
}

impl SymbolTable {
    /// Constructs a new symbol table with the global scope and runtime intrinsics initialized.
    pub fn new() -> Self {
        let mut table = Self {
            scope_tree: ScopeTree::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
        };
        table.register_runtime_intrinsics();
        table
    }

    /// Registers built-in console, file I/O, and mathematical runtime intrinsics.
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

    /// Pushes a new lexical scope.
    #[inline]
    pub fn push_scope(&self) {
        self.scope_tree.push_scope();
    }

    /// Pops the outermost scope if more than one scope exists.
    #[inline]
    pub fn pop_scope(&self) {
        self.scope_tree.pop_scope();
    }

    /// Inserts a variable symbol into the current scope.
    #[inline]
    pub fn insert(&self, name: String, ty: Type, is_mutable: bool) {
        self.scope_tree.insert(name, ty, is_mutable);
    }

    /// Updates the type of an existing symbol in the innermost scope containing it.
    #[inline]
    pub fn update_type(&self, name: &str, new_ty: Type) -> bool {
        self.scope_tree.update_type(name, new_ty)
    }

    /// Looks up a variable symbol traversing scopes from innermost to outermost.
    #[inline]
    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        self.scope_tree.lookup(name)
    }

    /// Verifies that no variables in the current scope retain an unresolved `Type::Infer`.
    #[inline]
    pub fn verify_no_unresolved_infer_types<F>(&self, make_err: F) -> Result<()>
    where
        F: FnMut(&str) -> MatcError,
    {
        self.scope_tree.verify_no_unresolved_infer_types(make_err)
    }

    /// Inserts a function declaration.
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

    /// Looks up a function symbol by exact name, runtime intrinsic alias, or module path suffix.
    pub fn lookup_function(&self, name: &str) -> Option<&FunctionSymbol> {
        if let Some(f) = self.functions.get(name) {
            return Some(f);
        }
        let short_name = name.split("::").last().unwrap_or(name);
        let intrinsic_name = format!("_mat_rt_{}", short_name);
        if let Some(f) = self.functions.get(&intrinsic_name) {
            return Some(f);
        }

        Self::lookup_by_suffix(&self.functions, name, short_name)
    }

    /// Inserts a struct declaration.
    pub fn insert_struct(&mut self, name: String, fields: Vec<StructFieldSymbol>) {
        self.structs
            .insert(name.clone(), StructSymbol { name, fields });
    }

    /// Looks up a struct symbol by exact name or module path suffix.
    pub fn lookup_struct(&self, name: &str) -> Option<&StructSymbol> {
        if let Some(s) = self.structs.get(name) {
            return Some(s);
        }
        let short_name = name.split("::").last().unwrap_or(name);
        Self::lookup_by_suffix(&self.structs, name, short_name)
    }

    /// Inserts an enum declaration.
    pub fn insert_enum(&mut self, name: String, variants: Vec<EnumVariantSymbol>) {
        self.enums
            .insert(name.clone(), EnumSymbol { name, variants });
    }

    /// Looks up an enum symbol by exact name or module path suffix.
    pub fn lookup_enum(&self, name: &str) -> Option<&EnumSymbol> {
        if let Some(e) = self.enums.get(name) {
            return Some(e);
        }
        let short_name = name.split("::").last().unwrap_or(name);
        Self::lookup_by_suffix(&self.enums, name, short_name)
    }

    /// Shared helper looking up entries by qualified suffix or short name (Rule E: DRY).
    fn lookup_by_suffix<'m, T>(
        map: &'m HashMap<String, T>,
        name: &str,
        short_name: &str,
    ) -> Option<&'m T> {
        if name.contains("::") {
            let suffix = format!("::{}", name);
            if let Some(entry) = map
                .iter()
                .find(|(k, _)| k.ends_with(&suffix))
                .map(|(_, v)| v)
            {
                return Some(entry);
            }
            if let Some(entry) = map.get(short_name) {
                return Some(entry);
            }
            let short_suffix = format!("::{}", short_name);
            map.iter()
                .find(|(k, _)| k.ends_with(&short_suffix))
                .map(|(_, v)| v)
        } else {
            let suffix = format!("::{}", name);
            map.iter()
                .find(|(k, _)| k.as_str() == short_name || k.ends_with(&suffix))
                .map(|(_, v)| v)
        }
    }
}
