//! Module import resolution and AST merging.
//!
//! This module resolves `import` declarations recursively against standard library
//! embedded modules or relative local file paths, and combines parsed AST items.

use std::collections::HashSet;
use std::fs;
use std::path::Path;

use super::prefixing::prefix_module_items;
use crate::Result;
use crate::ast::{ImportDeclaration, Item};
use crate::embedded::get_embedded_std_file;
use crate::error::MatcError;
use crate::parser::Parser;

/// Resolves imports for a source file recursively and returns the combined items.
///
/// # Arguments
///
/// * `source_path` - Path to the source file being parsed.
/// * `root_dir` - Root directory used for resolving non-relative imports.
/// * `visited` - Set of already processed module keys to prevent cycles and duplicate definitions.
///
/// # Returns
///
/// A vector of all items resolved across the source and imported modules.
///
/// # Errors
///
/// Returns a [`MatcError`] if reading files, parsing, or finding imported modules fails.
pub fn resolve_and_parse_program(
    source_path: &Path,
    root_dir: &Path,
    visited: &mut HashSet<String>,
) -> Result<Vec<Item>> {
    let source_str = fs::read_to_string(source_path)?;
    let file_name = source_path.to_string_lossy().to_string();

    let mut parser = Parser::new(&file_name, &source_str);
    let ast = parser.parse_program()?;

    let mut combined_items = Vec::new();
    let parent_dir = source_path.parent().unwrap_or_else(|| Path::new("."));

    for item in ast.items {
        match item {
            Item::Import(import_decl) => {
                resolve_single_import(
                    &import_decl,
                    parent_dir,
                    root_dir,
                    visited,
                    &mut combined_items,
                )?;
            }
            Item::Function(_) | Item::Struct(_) | Item::Enum(_) | Item::GlobalLet(_) => {
                combined_items.push(item);
            }
        }
    }

    Ok(combined_items)
}

/// Resolves a single import declaration, dispatching to standard or local resolution.
fn resolve_single_import(
    import_decl: &ImportDeclaration,
    parent_dir: &Path,
    root_dir: &Path,
    visited: &mut HashSet<String>,
    combined_items: &mut Vec<Item>,
) -> Result<()> {
    let module_key = compute_module_key(import_decl, parent_dir, root_dir);

    if visited.contains(&module_key) {
        return Ok(());
    }
    visited.insert(module_key);

    let prefix = import_decl.path.last().cloned().unwrap_or_default();

    if import_decl.is_std {
        resolve_std_import(import_decl, &prefix, combined_items)
    } else {
        resolve_local_import(
            import_decl,
            parent_dir,
            root_dir,
            visited,
            &prefix,
            combined_items,
        )
    }
}

/// Computes a unique tracking key for a module to detect cycles and duplicates.
fn compute_module_key(
    import_decl: &ImportDeclaration,
    parent_dir: &Path,
    root_dir: &Path,
) -> String {
    let rel_path_str = import_decl.path.join("/");
    let rel_mat_path = format!("{}.mat", rel_path_str);

    if import_decl.is_std {
        format!("std::{}", import_decl.path.join("::"))
    } else {
        let candidate_current = parent_dir.join(&rel_mat_path);
        let candidate_root = root_dir.join(&rel_mat_path);

        if candidate_current.exists() {
            candidate_current.to_string_lossy().to_string()
        } else {
            candidate_root.to_string_lossy().to_string()
        }
    }
}

/// Resolves an embedded standard library import.
fn resolve_std_import(
    import_decl: &ImportDeclaration,
    prefix: &str,
    combined_items: &mut Vec<Item>,
) -> Result<()> {
    if let Some(std_source) = get_embedded_std_module(&import_decl.path) {
        let std_file_name = format!("std::{}", import_decl.path.join("::"));
        let mut std_parser = Parser::new(&std_file_name, std_source);
        let mut std_ast = std_parser.parse_program()?;
        prefix_module_items(&mut std_ast.items, prefix);
        combined_items.extend(std_ast.items);
        Ok(())
    } else {
        Err(MatcError::CodegenError(format!(
            "Standard library module 'std::{}' not found",
            import_decl.path.join("::")
        )))
    }
}

/// Resolves a local project file import.
fn resolve_local_import(
    import_decl: &ImportDeclaration,
    parent_dir: &Path,
    root_dir: &Path,
    visited: &mut HashSet<String>,
    prefix: &str,
    combined_items: &mut Vec<Item>,
) -> Result<()> {
    let rel_path_str = import_decl.path.join("/");
    let rel_mat_path = format!("{}.mat", rel_path_str);

    let candidate_current = parent_dir.join(&rel_mat_path);
    let candidate_root = root_dir.join(&rel_mat_path);

    let rel_file_path = if candidate_current.exists() {
        candidate_current
    } else if candidate_root.exists() {
        candidate_root
    } else {
        return Err(MatcError::CodegenError(format!(
            "Local module file '{}' not found",
            rel_mat_path
        )));
    };

    let mut imported_items = resolve_and_parse_program(&rel_file_path, root_dir, visited)?;
    prefix_module_items(&mut imported_items, prefix);
    combined_items.extend(imported_items);
    Ok(())
}

/// Retrieves an embedded standard library module's source string.
fn get_embedded_std_module(path: &[String]) -> Option<&'static str> {
    let key = format!("std::{}", path.join("::"));
    get_embedded_std_file(&key)
}
