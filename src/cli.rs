use clap::{Parser as ClapParser, Subcommand};
use inkwell::context::Context;
use std::fs;
use std::path::PathBuf;

use crate::Result;
use crate::codegen::CodegenEngine;
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
    Build {
        source: PathBuf,

        #[arg(short, long)]
        output: Option<PathBuf>,

        #[arg(long)]
        emit_llvm: bool,
    },
}

impl Cli {
    pub fn run(&self) -> Result<()> {
        match &self.command {
            Commands::Build {
                source, emit_llvm, ..
            } => {
                let source_code = fs::read_to_string(source)?;

                // 1. Parse AST
                let mut parser = Parser::new(&source_code);
                let ast = parser.parse_program()?;

                // 2. Semantic Analysis
                let mut analyzer = SemanticAnalyzer::new();
                analyzer.analyze(&ast)?;

                // 3. LLVM IR Generation
                let context = Context::create();
                let codegen = CodegenEngine::new(&context, "main_module");
                codegen.compile_program(&ast)?;

                if *emit_llvm {
                    println!("{}", codegen.emit_llvm_ir());
                }

                Ok(())
            }
        }
    }
}
