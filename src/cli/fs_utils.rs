//! Filesystem and directory utility functions for CLI tasks.
//!
//! This module provides functions to calculate total directory sizes and format byte
//! counts into human-readable strings.

use std::fs;
use std::path::Path;

/// Calculates the recursive total size in bytes of a file or directory tree.
///
/// # Arguments
///
/// * `path` - The file or directory path to measure.
///
/// # Returns
///
/// Total byte size across all contained files.
///
/// # Errors
///
/// Returns an [`std::io::Error`] if reading directory contents or file metadata fails.
pub fn calculate_dir_size(path: &Path) -> std::io::Result<u64> {
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

/// Formats a byte size into a human-readable display string (e.g. KiB, MiB, GiB).
///
/// # Arguments
///
/// * `bytes` - The number of bytes to format.
///
/// # Returns
///
/// A formatted string representation.
pub fn format_size(bytes: u64) -> String {
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
