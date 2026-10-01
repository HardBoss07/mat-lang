use clap::{Parser as ClapParser, Subcommand};
use inkwell::context::Context;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::Result;
use crate::ast::{Expression, Item, Program, Statement};
use crate::codegen::{CodegenEngine, link_object_file};
use crate::embedded::get_embedded_std_file;
use crate::error::MatcError;
use crate::parser::Parser;
use crate::semantic::SemanticAnalyzer;

#[derive(ClapParser, Debug)]
#[command(name = "matc", version, about = "Compiler for the mat language")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Build a .mat source file into an executable, object file, assembly, or LLVM IR
    Build {
        /// Source file path (.mat)
        source: PathBuf,

        /// Output destination path
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Emit LLVM IR file (.ll)
        #[arg(long)]
        emit_llvm: bool,

        /// Emit target assembly file (.s)
        #[arg(long)]
        emit_asm: bool,

        /// Emit native object file (.o)
        #[arg(long)]
        emit_obj: bool,
    },
    /// Compile and run a .mat source file directly
    Run {
        /// Source file path (.mat)
        source: PathBuf,

        /// Arguments to pass to the executed program
        #[arg(raw = true)]
        args: Vec<String>,
    },
    /// Remove generated output artifacts
    Clean,
}

impl Cli {
    pub fn run(&self) -> Result<()> {
        match &self.command {
            Commands::Build {
                source,
                output,
                emit_llvm,
                emit_asm,
                emit_obj,
            } => {
                self.build_target(source, output.as_deref(), *emit_llvm, *emit_asm, *emit_obj)?;
                Ok(())
            }
            Commands::Run { source, args } => {
                let stem = source.file_stem().unwrap_or_default().to_string_lossy();
                let out_dir = PathBuf::from("out");
                fs::create_dir_all(&out_dir)?;

                let exec_filename = if cfg!(target_os = "windows") {
                    format!("{}.exe", stem)
                } else {
                    stem.to_string()
                };
                let exec_path = out_dir.join(exec_filename);

                self.build_target(source, Some(&exec_path), false, false, false)?;

                let mut child = Command::new(&exec_path)
                    .args(args)
                    .spawn()
                    .map_err(MatcError::Io)?;

                let status = child.wait().map_err(MatcError::Io)?;

                if !status.success() {
                    std::process::exit(status.code().unwrap_or(1));
                }

                Ok(())
            }
            Commands::Clean => {
                let out_dir = PathBuf::from("out");
                if out_dir.exists() {
                    let total_bytes = calculate_dir_size(&out_dir)?;
                    fs::remove_dir_all(&out_dir)?;
                    println!(
                        "Removed {} ({})",
                        out_dir.display(),
                        format_size(total_bytes)
                    );
                } else {
                    println!("Nothing to clean");
                }
                Ok(())
            }
        }
    }

    fn build_target(
        &self,
        source: &Path,
        output: Option<&Path>,
        emit_llvm: bool,
        emit_asm: bool,
        emit_obj: bool,
    ) -> Result<()> {
        let source_canonical = source
            .canonicalize()
            .unwrap_or_else(|_| source.to_path_buf());
        let root_dir = source_canonical.parent().unwrap_or_else(|| Path::new("."));

        let mut visited = HashSet::new();
        let items = resolve_and_parse_program(&source_canonical, root_dir, &mut visited)?;
        let ast = Program { items };

        let source_code = fs::read_to_string(source)?;
        let file_name = source.to_string_lossy();

        let mut analyzer = SemanticAnalyzer::new(&file_name, &source_code);
        analyzer.analyze(&ast)?;

        let context = Context::create();
        let module_name = source
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main_module");
        let codegen = CodegenEngine::new(&context, module_name);
        codegen.compile_program(&ast)?;

        let stem = source.file_stem().unwrap_or_default().to_string_lossy();
        let out_dir = PathBuf::from("out");
        fs::create_dir_all(&out_dir)?;

        if emit_llvm {
            let llvm_path = out_dir.join(format!("{}.ll", stem));
            codegen.write_llvm_ir_to_file(&llvm_path)?;
            println!("Emitted LLVM IR: {}", llvm_path.display());
        }

        if emit_asm {
            let asm_path = out_dir.join(format!("{}.s", stem));
            codegen.write_assembly_to_file(&asm_path)?;
            println!("Emitted Assembly: {}", asm_path.display());
        }

        let obj_path = out_dir.join(format!("{}.o", stem));
        codegen.write_object_to_file(&obj_path)?;

        if emit_obj && output.is_none() {
            println!("Emitted Object File: {}", obj_path.display());
            return Ok(());
        }

        if output.is_some() || (!emit_llvm && !emit_asm && !emit_obj) {
            let default_exec_name = if cfg!(target_os = "windows") {
                format!("{}.exe", stem)
            } else {
                stem.to_string()
            };

            let final_exec_path = output
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| out_dir.join(default_exec_name));

            link_object_file(&obj_path, &final_exec_path)?;

            if !emit_obj {
                let _ = fs::remove_file(&obj_path);
            }

            println!("Built Executable: {}", final_exec_path.display());
        } else if !emit_obj {
            let _ = fs::remove_file(&obj_path);
        }

        Ok(())
    }
}

fn resolve_and_parse_program(
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
                let rel_path_str = import_decl.path.join("/");
                let rel_mat_path = format!("{}.mat", rel_path_str);

                let module_key = if import_decl.is_std {
                    format!("std::{}", import_decl.path.join("::"))
                } else {
                    let candidate_current = parent_dir.join(&rel_mat_path);
                    let candidate_root = root_dir.join(&rel_mat_path);

                    if candidate_current.exists() {
                        candidate_current.to_string_lossy().to_string()
                    } else {
                        candidate_root.to_string_lossy().to_string()
                    }
                };

                if visited.contains(&module_key) {
                    continue;
                }
                visited.insert(module_key);

                let prefix = import_decl.path.last().cloned().unwrap_or_default();

                if import_decl.is_std {
                    if let Some(std_source) = get_embedded_std_module(&import_decl.path) {
                        let std_file_name = format!("std::{}", import_decl.path.join("::"));
                        let mut std_parser = Parser::new(&std_file_name, std_source);
                        let mut std_ast = std_parser.parse_program()?;
                        prefix_module_items(&mut std_ast.items, &prefix);
                        combined_items.extend(std_ast.items);
                    } else {
                        return Err(MatcError::CodegenError(format!(
                            "Standard library module 'std::{}' not found",
                            import_decl.path.join("::")
                        )));
                    }
                } else {
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

                    let mut imported_items =
                        resolve_and_parse_program(&rel_file_path, root_dir, visited)?;
                    prefix_module_items(&mut imported_items, &prefix);
                    combined_items.extend(imported_items);
                }
            }
            Item::Function(_) => {
                combined_items.push(item);
            }
        }
    }

    Ok(combined_items)
}

fn get_embedded_std_module(path: &[String]) -> Option<&'static str> {
    let key = format!("std::{}", path.join("::"));
    get_embedded_std_file(&key)
}

fn prefix_module_items(items: &mut [Item], prefix: &str) {
    for item in items {
        if let Item::Function(func) = item {
            if !func.name.contains("::") {
                func.name = format!("{}::{}", prefix, func.name);
            }
            for stmt in &mut func.body {
                prefix_statement(stmt, prefix);
            }
        }
    }
}

fn prefix_statement(stmt: &mut Statement, prefix: &str) {
    match stmt {
        Statement::Let { value, .. }
        | Statement::Assignment { value, .. }
        | Statement::CompoundAssignment { value, .. } => prefix_expression(value, prefix),
        Statement::Increment { .. } | Statement::Decrement { .. } => {}
        Statement::Loop { body, .. } => {
            for s in body {
                prefix_statement(s, prefix);
            }
        }
        Statement::While {
            condition, body, ..
        } => {
            prefix_expression(condition, prefix);
            for s in body {
                prefix_statement(s, prefix);
            }
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
            for s in body {
                prefix_statement(s, prefix);
            }
        }
        Statement::ForIn { iterable, body, .. } => {
            prefix_expression(iterable, prefix);
            for s in body {
                prefix_statement(s, prefix);
            }
        }
        Statement::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            prefix_expression(condition, prefix);
            for s in then_branch {
                prefix_statement(s, prefix);
            }
            if let Some(eb) = else_branch {
                for s in eb {
                    prefix_statement(s, prefix);
                }
            }
        }
        Statement::Match { expr, arms, .. } => {
            prefix_expression(expr, prefix);
            for arm in arms {
                for s in &mut arm.body {
                    prefix_statement(s, prefix);
                }
            }
        }
        Statement::Return(opt_expr, _) => {
            if let Some(e) = opt_expr {
                prefix_expression(e, prefix);
            }
        }
        Statement::Break(_) | Statement::Continue(_) => {}
        Statement::Expression(expr) => prefix_expression(expr, prefix),
    }
}

fn prefix_expression(expr: &mut Expression, prefix: &str) {
    match expr {
        Expression::Call {
            callee, arguments, ..
        } => {
            if !callee.contains("::")
                && callee != "println"
                && callee != "print"
                && !callee.starts_with("_mat_rt_")
            {
                *callee = format!("{}::{}", prefix, callee);
            }
            for arg in arguments {
                prefix_expression(arg, prefix);
            }
        }
        Expression::Binary { left, right, .. } => {
            prefix_expression(left, prefix);
            prefix_expression(right, prefix);
        }
        Expression::TupleLiteral(elems, _) | Expression::ArrayLiteral(elems, _) => {
            for elem in elems {
                prefix_expression(elem, prefix);
            }
        }
        Expression::InterpolatedString(parts, _) => {
            for (part, _) in parts {
                prefix_expression(part, prefix);
            }
        }
        Expression::Ok(e, _) | Expression::Err(e, _) => prefix_expression(e, prefix),
        Expression::TupleAccess { expr, .. } => prefix_expression(expr, prefix),
        Expression::ArrayAccess { expr, index, .. } => {
            prefix_expression(expr, prefix);
            prefix_expression(index, prefix);
        }
        _ => {}
    }
}

fn calculate_dir_size(path: &Path) -> std::io::Result<u64> {
    let mut total_size = 0;
    if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let metadata = entry.metadata()?;
            if metadata.is_dir() {
                total_size += calculate_dir_size(&entry.path())?;
            } else {
                total_size += metadata.len();
            }
        }
    } else {
        total_size += fs::metadata(path)?.len();
    }
    Ok(total_size)
}

fn format_size(bytes: u64) -> String {
    const KIB: u64 = 1024;
    const MIB: u64 = KIB * 1024;
    const GIB: u64 = MIB * 1024;

    if bytes >= GIB {
        format!("{:.2} GiB", bytes as f64 / GIB as f64)
    } else if bytes >= MIB {
        format!("{:.2} MiB", bytes as f64 / MIB as f64)
    } else if bytes >= KIB {
        format!("{:.2} KiB", bytes as f64 / KIB as f64)
    } else {
        format!("{} B", bytes)
    }
}
