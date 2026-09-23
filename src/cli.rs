use crate::Result;
use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(
    name = "matc",
    version,
    about = "Compiler for the mat programming language"
)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,

    #[arg(short, long, global = true)]
    pub verbose: bool,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Compile a .mat file to an executable
    Build {
        /// Input source file path (.mat)
        source: PathBuf,

        /// Output binary destination path
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Emit LLVM IR file (.ll) alongside executable
        #[arg(long)]
        emit_llvm: bool,
    },
    /// Run source file directly
    Run { source: PathBuf },
}

impl Cli {
    pub fn run(&self) -> Result<()> {
        match &self.command {
            Commands::Build {
                source,
                output,
                emit_llvm,
            } => {
                tracing::info!(file = ?source, emit_llvm = %emit_llvm, "Building target");
                Ok(())
            }
            Commands::Run { source } => {
                tracing::info!(file = ?source, "Executing target");
                Ok(())
            }
        }
    }
}
