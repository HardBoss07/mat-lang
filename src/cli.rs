//! Command-line interface definitions and command handling for `matc`.
//!
//! This module defines CLI options and entry points for building, running,
//! and cleaning MAT language projects.

pub mod builder;
pub mod fs_utils;
pub mod prefixing;
pub mod resolver;

pub use builder::*;
pub use fs_utils::*;
pub use prefixing::*;
pub use resolver::*;

use clap::{Parser as ClapParser, Subcommand};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::Result;
use crate::error::MatcError;

/// Top-level command line arguments parser for the MAT compiler.
#[derive(ClapParser, Debug)]
#[command(name = "matc", version, about = "Compiler for the mat language")]
pub struct Cli {
    /// Subcommand to execute.
    #[command(subcommand)]
    pub command: Commands,
}

/// Available subcommands supported by `matc`.
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
    /// Executes the parsed CLI subcommand.
    ///
    /// # Returns
    ///
    /// `Ok(())` on successful completion.
    ///
    /// # Errors
    ///
    /// Returns a [`MatcError`] if any compilation, execution, or I/O step fails.
    pub fn run(&self) -> Result<()> {
        match &self.command {
            Commands::Build {
                source,
                output,
                emit_llvm,
                emit_asm,
                emit_obj,
            } => {
                let options = BuildOptions {
                    emit_llvm: *emit_llvm,
                    emit_asm: *emit_asm,
                    emit_obj: *emit_obj,
                };
                build_target(source, output.as_deref(), options)
            }
            Commands::Run { source, args } => self.run_target(source, args),
            Commands::Clean => self.clean_output_directory(),
        }
    }

    /// Compiles a source file and executes the generated binary directly.
    fn run_target(&self, source: &Path, args: &[String]) -> Result<()> {
        let stem = source.file_stem().unwrap_or_default().to_string_lossy();
        let out_dir = PathBuf::from("out");
        fs::create_dir_all(&out_dir)?;

        let exec_filename = if cfg!(target_os = "windows") {
            format!("{}.exe", stem)
        } else {
            stem.to_string()
        };
        let exec_path = out_dir.join(exec_filename);

        let options = BuildOptions {
            emit_llvm: false,
            emit_asm: false,
            emit_obj: false,
        };
        build_target(source, Some(&exec_path), options)?;

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

    /// Cleans generated build artifacts from the output directory.
    fn clean_output_directory(&self) -> Result<()> {
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
