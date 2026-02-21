/*
 * Copyright 2026 James Carl
 * AGPL-3.0-only or AGPL-3.0-or-later
 *
 * This file is part of Command Cad.
 *
 * Command CAD is free software: you can redistribute it and/or modify it under the terms of
 * the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <https://www.gnu.org/licenses/>.
 */

use std::{
    io::{ErrorKind, Write},
    path::{Path, PathBuf},
};

use sha2::{Digest, Sha256};
use tempfile::{NamedTempFile, TempDir};

use crate::{
    execution::errors::{ErrorType, ExecutionResult, Raise},
    ExecutionContext,
};

#[derive(Debug)]
pub struct Store {
    path: PathBuf,
}

impl Store {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self { path: path.into() }
    }

    pub fn get_or_init_file(
        &self,
        context: &ExecutionContext,
        hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        init: impl FnOnce(&mut NamedTempFile) -> ExecutionResult<()>,
    ) -> ExecutionResult<PathBuf> {
        let store_path = self.generate_store_path(hashable, &name);

        if std::fs::exists(&store_path)
            .map_err(|error| IoError(error).to_error(context.stack_trace))?
        {
            Ok(store_path)
        } else {
            // TODO should we be creating these in the project directory to increase the chances of
            // them being on the same filesystem as the store?
            let mut asset = PendingAsset {
                store_path,
                asset: NamedTempFile::new()
                    .map_err(|error| IoError(error).to_error(context.stack_trace))?,
            };
            init(&mut asset.asset)?;

            let (mut file, temp_path) = asset
                .asset
                .keep()
                .map_err(|error| IoError(error.error).to_error(context.stack_trace))?;

            // Make sure that file is flushed and closed.
            file.flush()
                .map_err(|error| IoError(error).to_error(context.stack_trace))?;
            drop(file);

            self.move_path_into_store(context, &temp_path, &asset.store_path)?;

            Ok(asset.store_path)
        }
    }

    pub fn get_or_init_directory(
        &self,
        context: &ExecutionContext,
        hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        init: impl FnOnce(&mut TempDir) -> ExecutionResult<()>,
    ) -> ExecutionResult<PathBuf> {
        let store_path = self.generate_store_path(hashable, &name);

        if std::fs::exists(&store_path)
            .map_err(|error| IoError(error).to_error(context.stack_trace))?
        {
            Ok(store_path)
        } else {
            // TODO should we be creating these in the project directory to increase the chances of
            // them being on the same filesystem as the store?
            let mut asset = PendingAsset {
                store_path,
                asset: TempDir::new()
                    .map_err(|error| IoError(error).to_error(context.stack_trace))?,
            };
            init(&mut asset.asset)?;
            let temp_path = asset.asset.keep();
            self.move_path_into_store(context, &temp_path, &asset.store_path)?;

            Ok(asset.store_path)
        }
    }

    fn generate_store_path(
        &self,
        hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
    ) -> PathBuf {
        let mut hasher = StoreHasher(Sha256::new());
        hashable.hash(&mut hasher);
        let hash = hasher.0.finalize();
        let file_name = {
            let mut file_name = hex::encode(&hash[..]);
            file_name += "-";
            file_name += name.as_ref();
            file_name
        };
        self.path.join(file_name)
    }

    fn move_path_into_store(
        &self,
        context: &ExecutionContext,
        temp_path: &Path,
        store_path: &Path,
    ) -> ExecutionResult<()> {
        // Move the file into the store.
        match std::fs::rename(temp_path, store_path) {
            Ok(_) => Ok(store_path),
            Err(error) => {
                let result = match error.kind() {
                    // Seems another thread created the object while we were working.
                    // That's unfortunate, but we can just use their result.
                    ErrorKind::AlreadyExists => Ok(store_path),
                    _ => Err(error),
                };

                // Either way, we need to remove that old directory.
                if let Err(error) = std::fs::remove_dir_all(temp_path) {
                    // This just really isn't our day...
                    context.log.push_message(super::LogMessage {
                        origin: context.stack_trace.bottom().clone(),
                        level: super::LogLevel::Warning,
                        message: format!(
                            "Failed to remove temporary file at {temp_path:?}: {error}"
                        )
                        .into(),
                    });
                }

                result
            }
        }
        .map_err(|error| IoError(error).to_error(context.stack_trace))?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct PendingAsset<A> {
    store_path: PathBuf,
    asset: A,
}

impl<A> std::ops::Deref for PendingAsset<A> {
    type Target = A;

    fn deref(&self) -> &Self::Target {
        &self.asset
    }
}

impl<A> std::ops::DerefMut for PendingAsset<A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.asset
    }
}

#[derive(Debug)]
pub struct IoError(pub std::io::Error);

impl ErrorType for IoError {}

impl std::fmt::Display for IoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

struct StoreHasher(Sha256);

impl std::hash::Hasher for StoreHasher {
    fn finish(&self) -> u64 {
        // Use the Sha256 built in to get the real hash.
        0
    }

    fn write(&mut self, bytes: &[u8]) {
        Digest::update(&mut self.0, bytes);
    }
}
