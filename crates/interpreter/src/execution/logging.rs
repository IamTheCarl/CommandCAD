use std::{
    borrow::Cow,
    fmt::Display,
    ops::{Deref, DerefMut},
    path::PathBuf,
    sync::Arc,
};

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

/// A string that knows what file it came from.
pub struct LocatedString {
    pub location: StackPoint,
    pub string: String,
}

impl Deref for LocatedString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl DerefMut for LocatedString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.string
    }
}

impl AsRef<str> for LocatedString {
    fn as_ref(&self) -> &str {
        self.string.as_ref()
    }
}

/// A str that knows what file it came from.
pub struct LocatedStr<'s> {
    pub location: StackPoint,
    pub string: &'s str,
}

impl<'s> Deref for LocatedStr<'s> {
    type Target = &'s str;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl<'s> From<&'s LocatedString> for LocatedStr<'s> {
    fn from(value: &'s LocatedString) -> Self {
        Self {
            location: value.location.clone(),
            string: &value.string,
        }
    }
}

impl<'s> From<&'s LocatedStr<'s>> for LocatedStr<'s> {
    fn from(value: &'s LocatedStr<'s>) -> Self {
        Self {
            location: value.location.clone(),
            string: &value.string,
        }
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
