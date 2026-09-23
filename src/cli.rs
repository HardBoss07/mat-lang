use clap::{Parser as ClapParser, Subcommand};
use inkwell::context::Context;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::Result;
use crate::codegen::{CodegenEngine, link_object_file};
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
        let source_code = fs::read_to_string(source)?;

        let mut parser = Parser::new(&source_code);
        let ast = parser.parse_program()?;

        let mut analyzer = SemanticAnalyzer::new();
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
