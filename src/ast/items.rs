//! Top-level item and program declarations in the Abstract Syntax Tree.
//!
//! This module defines functions, imports, structs, enums, global variable declarations,
//! and the root [`Program`] node.

use serde::Serialize;

use super::expressions::Expression;
use super::span::Span;
use super::statements::Statement;
use super::types::Type;

/// Function parameter specification including name, mutability, and type.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Param {
    pub is_mutable: bool,
    pub name: String,
    pub ty: Type,
    #[serde(skip)]
    pub span: Span,
}

/// Function declaration node.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FunctionDeclaration {
    pub is_pub: bool,
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Vec<Statement>,
    #[serde(skip)]
    pub span: Span,
}

/// Module import declaration node.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ImportDeclaration {
    pub is_std: bool,
    pub path: Vec<String>,
    #[serde(skip)]
    pub span: Span,
}

/// Struct field declaration.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StructField {
    pub is_pub: bool,
    pub name: String,
    pub ty: Type,
    #[serde(skip)]
    pub span: Span,
}

/// Struct declaration node.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StructDeclaration {
    pub is_pub: bool,
    pub name: String,
    pub fields: Vec<StructField>,
    #[serde(skip)]
    pub span: Span,
}

/// Enum variant payload (unit, tuple, or struct).
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum EnumVariantPayload {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<StructField>),
}

/// Individual enum variant declaration.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnumVariant {
    pub name: String,
    pub payload: EnumVariantPayload,
    #[serde(skip)]
    pub span: Span,
}

/// Enum declaration node.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct EnumDeclaration {
    pub is_pub: bool,
    pub name: String,
    pub variants: Vec<EnumVariant>,
    #[serde(skip)]
    pub span: Span,
}

/// Top-level global let declaration statement.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct GlobalLetDeclaration {
    pub is_pub: bool,
    pub name: String,
    pub is_mutable: bool,
    pub ty: Option<Type>,
    pub value: Expression,
    #[serde(skip)]
    pub span: Span,
}

/// Top-level AST item.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Item {
    Function(FunctionDeclaration),
    Import(ImportDeclaration),
    Struct(StructDeclaration),
    Enum(EnumDeclaration),
    GlobalLet(GlobalLetDeclaration),
}

/// Root program AST node consisting of items.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Program {
    pub items: Vec<Item>,
}
