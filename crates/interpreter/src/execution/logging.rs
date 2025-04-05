use std::{borrow::Cow, fmt::Display, path::PathBuf, sync::Arc};

use tree_sitter::Point as FilePoint;

pub trait RuntimeLog {
    fn push(&mut self, message: LogMessage);
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StackPoint {
    pub file: Arc<PathBuf>,
    pub file_point: FilePoint,
}

impl Display for StackPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file.to_string_lossy(),
            // Most IDEs expect the first line to be 1.
            self.file_point.row + 1,
            self.file_point.column + 1
        )
    }
}

#[derive(Debug)]
pub struct LogMessage {
    pub origin: StackPoint,
    pub level: LogLevel,
    pub message: Cow<'static, str>,
}

impl std::fmt::Display for LogMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.origin, self.message)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum LogLevel {
    Info,
    Warning,
}
