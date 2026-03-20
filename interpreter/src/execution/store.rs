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
    io::{BufReader, BufWriter, ErrorKind, Write},
    path::{Path, PathBuf},
};

use enum_dispatch::enum_dispatch;
use serde::{de::DeserializeOwned, Serialize};
use sha2::{Digest, Sha256};
use tempfile::{NamedTempFile, TempDir};

use crate::{
    execution::errors::{ExecutionResult, Raise, StringError},
    ExecutionContext,
};

#[enum_dispatch]
pub trait StoreTrait {
    fn get_or_init_file(
        &self,
        context: &ExecutionContext,
        hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        init: impl FnOnce(&mut NamedTempFile) -> ExecutionResult<()>,
    ) -> ExecutionResult<PathBuf>;

    fn get_or_init_object<S>(
        &self,
        context: &ExecutionContext,
        hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        init: impl FnOnce() -> ExecutionResult<S>,
    ) -> ExecutionResult<S>
    where
        S: Serialize + DeserializeOwned,
    {
        let mut object = None;
        let file_path = self.get_or_init_file(context, hashable, name, |file| {
            let new_object = init()?;
            let mut buf_writer = BufWriter::new(file);
            ciborium::into_writer(&new_object, &mut buf_writer)
                .map_err(|error| error.to_error(context))?;
            buf_writer
                .flush()
                .map_err(|error| error.to_error(context))?;

            object = Some(new_object);

            Ok(())
        })?;

        if let Some(object) = object {
            Ok(object)
        } else {
            let file = std::fs::File::open(file_path).map_err(|error| error.to_error(context))?;
            let reader = BufReader::new(file);
            let object = ciborium::from_reader(reader).map_err(|error| error.to_error(context))?;
            Ok(object)
        }
    }

    fn get_or_init_directory(
        &self,
        context: &ExecutionContext,
        hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        init: impl FnOnce(&mut TempDir) -> ExecutionResult<()>,
    ) -> ExecutionResult<PathBuf>;
}

#[enum_dispatch(StoreTrait)]
#[derive(Debug)]
pub enum Store {
    FsStore,
    DummyStore,
}

#[derive(Debug)]
pub struct DummyStore;

impl StoreTrait for DummyStore {
    fn get_or_init_file(
        &self,
        context: &ExecutionContext,
        _hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        _init: impl FnOnce(&mut NamedTempFile) -> ExecutionResult<()>,
    ) -> ExecutionResult<PathBuf> {
        Err(StringError(format!(
            "Cannot store a file with a dummy store: {}",
            name.as_ref()
        ))
        .to_error(context))
    }

    fn get_or_init_directory(
        &self,
        context: &ExecutionContext,
        _hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        _init: impl FnOnce(&mut TempDir) -> ExecutionResult<()>,
    ) -> ExecutionResult<PathBuf> {
        Err(StringError(format!(
            "Cannot store a directory with a dummy store: {}",
            name.as_ref()
        ))
        .to_error(context))
    }
}

#[derive(Debug)]
pub struct FsStore {
    /// Path to the store itself.
    path: PathBuf,

    /// Temporary directory within the store.
    /// Is put into the parent directory of the store because it MUST live on the same filesystem
    /// as the store itself, otherwise transferring files into the store may fail.
    temp_dir: PathBuf,
}

impl FsStore {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        let temp_dir = path.join("../temp");
        Self { path, temp_dir }
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
        .map_err(|error| error.to_error(context))?;

        Ok(())
    }
}

impl StoreTrait for FsStore {
    fn get_or_init_file(
        &self,
        context: &ExecutionContext,
        hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        init: impl FnOnce(&mut NamedTempFile) -> ExecutionResult<()>,
    ) -> ExecutionResult<PathBuf> {
        let name = name.as_ref();

        context.trace_scope(
            Some(format!("Failed to fetch or create file {name} in store").into()),
            context.stack_trace.bottom().clone(),
            |context| {
                let store_path = self.generate_store_path(hashable, name);

                if std::fs::exists(&store_path).map_err(|error| error.to_error(context))? {
                    Ok(store_path)
                } else {
                    std::fs::create_dir_all(&self.temp_dir)
                        .map_err(|error| error.to_error(context))?;
                    let mut asset = PendingAsset {
                        store_path,
                        asset: NamedTempFile::new_in(&self.temp_dir)
                            .map_err(|error| error.to_error(context))?,
                    };
                    init(&mut asset.asset)?;

                    let (mut file, temp_path) = asset
                        .asset
                        .keep()
                        .map_err(|error| error.error.to_error(context))?;

                    // Make sure that file is flushed and closed.
                    file.flush().map_err(|error| error.to_error(context))?;
                    drop(file);

                    self.move_path_into_store(context, &temp_path, &asset.store_path)?;

                    Ok(asset.store_path)
                }
            },
        )
    }

    fn get_or_init_directory(
        &self,
        context: &ExecutionContext,
        hashable: &impl std::hash::Hash,
        name: impl AsRef<str>,
        init: impl FnOnce(&mut TempDir) -> ExecutionResult<()>,
    ) -> ExecutionResult<PathBuf> {
        let name = name.as_ref();

        context.trace_scope(
            Some(format!("Failed to fetch or create directory {name} in store").into()),
            context.stack_trace.bottom().clone(),
            |context| {
                let store_path = self.generate_store_path(hashable, name);

                if std::fs::exists(&store_path).map_err(|error| error.to_error(context))? {
                    Ok(store_path)
                } else {
                    std::fs::create_dir_all(&self.temp_dir)
                        .map_err(|error| error.to_error(context))?;
                    let mut asset = PendingAsset {
                        store_path,
                        asset: TempDir::new_in(&self.temp_dir)
                            .map_err(|error| error.to_error(context))?,
                    };
                    init(&mut asset.asset)?;
                    let temp_path = asset.asset.keep();
                    self.move_path_into_store(context, &temp_path, &asset.store_path)?;

                    Ok(asset.store_path)
                }
            },
        )
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
