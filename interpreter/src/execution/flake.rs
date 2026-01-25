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
    borrow::Cow,
    collections::HashSet,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    sync::{Arc, OnceLock},
};

use enum_downcast::AsVariant;
use git2::{Cred, CredentialType, Direction, FetchOptions, Remote, RemoteCallbacks, Repository};
use git_url_parse::GitUrl;
use hashable_map::HashableMap;
use imstr::ImString;
use ordered_hash_map::OrderedHashMap;
use sha2::{Digest, Sha256};
use unwrap_enum::EnumAs;

use crate::execution::{errors::ErrorType, store::IoError};
use crate::{
    execution::errors::ExpressionResult,
    values::{closure::Signature, IString, StructDefinition},
};
use crate::{
    execution::errors::{GenericFailure, Raise},
    values::{Dictionary, StaticType, StaticTypeName, UserClosure, Value, ValueType},
};
use crate::{values::BuiltinCallableDatabase, ExecutionContext};
use serde::{Deserialize, Serialize};

/// Closure for building outputs.
struct Outputs(pub UserClosure);

impl StaticType for Outputs {
    fn static_type() -> ValueType {
        static TYPE: OnceLock<Arc<Signature>> = OnceLock::new();
        let signature = TYPE.get_or_init(|| {
            let members = Arc::new(HashableMap::new());

            Arc::new(Signature {
                argument_type: crate::execution::values::StructDefinition {
                    members,
                    variadic: false,
                },
                return_type: <Dictionary as StaticType>::static_type(),
            })
        });

        ValueType::Closure(signature.clone())
    }
}

impl StaticTypeName for Outputs {
    fn static_type_name() -> std::borrow::Cow<'static, str> {
        "Closure".into()
    }
}

impl enum_downcast::IntoVariant<Outputs> for Value {
    fn into_variant(self) -> Result<Outputs, Value> {
        Ok(Outputs(self.into_variant()?))
    }
}

impl Into<UserClosure> for Outputs {
    fn into(self) -> UserClosure {
        self.0
    }
}

impl std::ops::Deref for Outputs {
    type Target = UserClosure;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn register_methods(database: &mut BuiltinCallableDatabase) {}

struct Flake {
    inputs: OrderedHashMap<ImString, Flake>,
    source: InputNode,
    output_function: Outputs,
}

fn get_field_from_dictionary<'d, F>(
    context: &ExecutionContext,
    dictionary: &'d Dictionary,
    dictionary_name: &str,
    field_name: &str,
) -> ExpressionResult<&'d F>
where
    F: StaticTypeName,
    Value: AsVariant<F>,
{
    dictionary
        .get(field_name)
        .ok_or_else(|| {
            GenericFailure(
                format!("Missing field `{field_name}` for input `{dictionary_name}`").into(),
            )
            .to_error(context.stack_trace)
        })?
        .downcast_ref(context.stack_trace)
        .map_err(|mut error| {
            error.ty = Box::new(GenericFailure(
                format!(
                    "Wrong type found for {dictionary_name}.{field_name}: {}",
                    error.ty
                )
                .into(),
            ));
            error
        })
}

impl Flake {
    fn from_dictionary(
        context: &ExecutionContext,
        file_path: &Path,
        dictionary: &Dictionary,
    ) -> ExpressionResult<Self> {
        // Verify our inputs.
        {
            static SIGNATURE: OnceLock<StructDefinition> = OnceLock::new();
            let signature = SIGNATURE.get_or_init(
            || crate::build_struct_definition!(variadic: false, (inputs: Dictionary, outputs: Outputs)),
        );

            signature
                .check_other_qualifies(dictionary.struct_def())
                .map_err(|error| error.to_error(context.stack_trace))?;
        }

        // None of these error conditions should ever happen because of the signature check we just did.
        let raw_inputs: &Dictionary = dictionary
            .get("inputs")
            .ok_or_else(|| {
                GenericFailure("Inputs did not exist".into()).to_error(context.stack_trace)
            })?
            .downcast_ref(context.stack_trace)?;

        let mut inputs = OrderedHashMap::new();
        for (name, value) in raw_inputs.data.members.iter() {
            let input: &Dictionary =
                value
                    .downcast_ref(context.stack_trace)
                    .map_err(|mut error| {
                        error.ty = Box::new(GenericFailure(
                            format!("Wrong type found for input `{name}`: {}", error.ty).into(),
                        ));
                        error
                    })?;

            let flake = Self::from_input_dictionary(context, name, input)?;
            inputs.insert(name.clone(), flake);
        }

        // let mut collected_inputs = OrderedHashMap::new();
        // for (name, value) in inputs.data.members.iter() {
        //     let value = value.downcast_ref(context.stack_trace)?;
        //     let flake = Flake::from_input_dictionary(context, value)?;
        //     collected_inputs.insert(name.clone(), flake);
        // }

        // let source = InputNode {
        //     source: NodeSource::Local {
        //         path: file_path.clone(),
        //     },
        // };

        // let lock_file = file_path.parent().ok_or_else(|| {
        //     GenericFailure("Flake file directory does not have a parent".into())
        //         .to_error(context.stack_trace)
        // })?;

        // let root_flake = Flake {
        //     inputs: collected_inputs,
        //     source,
        //     output_function: outputs,
        // };

        // let lock_file = if lock_file.exists() {
        //     LockFile::read(context, &lock_file)?
        // } else {
        //     context.log.push_message(LogMessage {
        //         origin: context.stack_trace.bottom().clone(),
        //         level: crate::LogLevel::Warning,
        //         message: "Lock file did not exist. A new one will be created.".into(),
        //     });

        //     root_flake.lock(context, None)?
        // };

        // Ok(todo!())
        todo!()
    }

    fn from_input_dictionary(
        context: &ExecutionContext,
        name: &str,
        input: &Dictionary,
    ) -> ExpressionResult<Self> {
        let ty: &IString = get_field_from_dictionary(context, input, name, "type")?;

        let node_source = match ty.0.as_str() {
            "local" => {
                let path: &IString = get_field_from_dictionary(context, input, name, "path")?;
                let path = PathBuf::from(path.0.as_str());

                InputNode {
                    source: NodeSource::Local { path },
                }
            }
            "git" => {
                let url: &IString = get_field_from_dictionary(context, input, name, "url")?;
                let branch = input.get("branch");
                let commit = input.get("commit");

                let reference = match (branch, commit) {
                    (Some(branch), Option::None) => {
                        let branch: &IString =
                            branch.downcast_ref(context.stack_trace).map_err(|error| {
                                GenericFailure(
                                    format!(
                                    "Git input {name}.branch was not of the expected type: {error}"
                                )
                                    .into(),
                                )
                                .to_error(context.stack_trace)
                            })?;

                        GitReference::Branch(branch.0.to_string())
                    }
                    (Option::None, Some(commit)) => {
                        let commit: &IString =
                            commit.downcast_ref(context.stack_trace).map_err(|error| {
                                GenericFailure(
                                    format!(
                                    "Git input {name}.commit was not of the expected type: {error}"
                                )
                                    .into(),
                                )
                                .to_error(context.stack_trace)
                            })?;

                        GitReference::Commit(commit.0.to_string())
                    }
                    (Option::None, Option::None) => GitReference::Default,
                    (Some(_), Some(_)) => {
                        return Err(GenericFailure(
                            format!("Git input {name} cannot reference both a branch and a commit")
                                .into(),
                        )
                        .to_error(context.stack_trace))
                    }
                };

                InputNode {
                    source: NodeSource::Git {
                        remote: url.0.to_string(),
                        reference,
                    },
                }
            }
            ty => {
                return Err(GenericFailure(
                    format!("Unsupported input type {ty} for input {name}").into(),
                )
                .to_error(context.stack_trace))
            }
        };

        todo!()
    }

    fn lock(
        &self,
        context: &ExecutionContext,
        original_lockfile: Option<LockFile>,
    ) -> ExpressionResult<LockFile> {
        let root = PathBuf::from("/");
        let mut to_keep = HashSet::new();

        let mut lockfile = original_lockfile.unwrap_or_else(|| LockFile::default());
        self.lock_internal(context, &root, &mut to_keep, &mut lockfile)?;

        // Remove any locked nodes that are no longer in the inputs list.
        // OrderedHashMap for some reason does not have a `retain` method.
        let to_remove: Vec<_> = lockfile
            .nodes
            .iter()
            .filter(|(name, _node)| to_keep.contains(name.as_path()))
            .map(|(name, _node)| name.clone())
            .collect();
        for name in to_remove {
            lockfile.nodes.remove(name.as_path());
        }

        Ok(lockfile)
    }

    fn lock_internal(
        &self,
        context: &ExecutionContext,
        root: &Path,
        to_keep: &mut HashSet<PathBuf>,
        lockfile: &mut LockFile,
    ) -> ExpressionResult<()> {
        for (name, flake) in self.inputs.iter() {
            let path = root.join(name);
            to_keep.insert(path.clone());
            flake.lock_internal(context, &path, to_keep, lockfile)?;
        }

        let node = self.source.lock(context)?;
        lockfile.nodes.insert(root.into(), node);

        Ok(())
    }
}

struct InputNode {
    source: NodeSource,
}

impl InputNode {
    fn lock(&self, context: &ExecutionContext) -> ExpressionResult<LockedNode> {
        let source = self.source.lock(context)?;
        let source_code = source.fetch_source(context)?;
        let source_hash = hash_filesystem(context, git_repo_predicate, source_code)?;

        Ok(LockedNode {
            source,
            source_hash,
        })
    }
}

#[derive(Debug, Hash, Serialize, Deserialize, EnumAs)]
enum LockedNodeSource {
    Git { remote: String, commit: String },
    Local { path: PathBuf },
}

impl LockedNodeSource {
    /// Fetches the source. Note that this DOES NOT verify the content.
    fn fetch_source(&self, context: &ExecutionContext) -> ExpressionResult<PathBuf> {
        // TODO we should give some kind of progress feedback to the caller.

        match self {
            Self::Git { remote, commit } => context.store.get_or_init_directory(
                context,
                self,
                format!("{remote}-{commit}"),
                |directory| {
                    let repository = Repository::init(directory.path())
                        .map_err(|error| GitError(error).to_error(context.stack_trace))?;
                    let mut remote = repository
                        .remote("origin", remote.as_str())
                        .map_err(|error| GitError(error).to_error(context.stack_trace))?;

                    // TODO we should probably log and report progress.
                    let mut remote_callbacks = RemoteCallbacks::new();
                    remote_callbacks.credentials(credential_callback);

                    let mut fetch_options = FetchOptions::new();
                    fetch_options.depth(1);
                    fetch_options.remote_callbacks(remote_callbacks);

                    remote
                        .fetch(&[commit], Some(&mut fetch_options), None)
                        .map_err(|error| GitError(error).to_error(context.stack_trace))?;

                    Ok(())
                },
            ),
            Self::Local { path } => Ok(path.clone()),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct LockedNode {
    source: LockedNodeSource,
    source_hash: String,
}

#[derive(Debug, Serialize, Deserialize, Default)]
struct LockFile {
    nodes: OrderedHashMap<PathBuf, LockedNode>,
}

impl LockFile {
    fn read(context: &ExecutionContext, path: &Path) -> ExpressionResult<Self> {
        let reader = std::io::BufReader::new(std::fs::File::open(path).map_err(|error| {
            GenericFailure(format!("Failed to open lock file: {error}").into())
                .to_error(context.stack_trace)
        })?);

        let lock_file: LockFile = serde_json::from_reader(reader).map_err(|error| {
            GenericFailure(format!("Failed to deserialize lock file: {error}").into())
                .to_error(context.stack_trace)
        })?;

        Ok(lock_file)
    }
}

#[derive(Debug, Hash, Serialize, Deserialize, Clone)]
enum NodeSource {
    Git {
        remote: String,
        reference: GitReference,
    },
    Local {
        path: PathBuf,
    },
}

impl NodeSource {
    fn lock(&self, context: &ExecutionContext) -> ExpressionResult<LockedNodeSource> {
        match self {
            Self::Git { remote, reference } => {
                let url = GitUrl::parse(remote).map_err(|error| {
                    GenericFailure(format!("Failed to parse Git remote URL: {error}").into())
                        .to_error(context.stack_trace)
                })?;
                let commit = reference.fetch_head_commit(context, &url)?;

                Ok(LockedNodeSource::Git {
                    remote: remote.clone(),
                    commit,
                })
            }
            Self::Local { path } => Ok(LockedNodeSource::Local { path: path.clone() }),
        }
    }
}

const HASHING_BUFFER_SIZE: usize = 1024 * 4;

fn hash_filesystem(
    context: &ExecutionContext,
    predicate: impl Fn(&Path) -> bool,
    path: impl AsRef<Path>,
) -> ExpressionResult<String> {
    let mut buffer = vec![0u8; HASHING_BUFFER_SIZE];
    let mut digest = Sha256::new();

    hash_filesystem_branch(context, &mut digest, &mut buffer, &predicate, path)?;

    let hash = digest.finalize();
    Ok(hex::encode(&hash[..]))
}

fn hash_filesystem_branch(
    context: &ExecutionContext,
    digest: &mut impl Digest,
    buffer: &mut [u8],
    predicate: &impl Fn(&Path) -> bool,
    path: impl AsRef<Path>,
) -> ExpressionResult<()> {
    let path = path.as_ref();

    if predicate(path) {
        let metadata = path
            .metadata()
            .map_err(|error| IoError(error).to_error(context.stack_trace))?;

        const DIRECTORY: (bool, bool, bool) = (true, false, false);
        const FILE: (bool, bool, bool) = (false, true, false);
        const SYMLINK: (bool, bool, bool) = (false, false, true);

        let file_type = metadata.file_type();
        match (
            file_type.is_dir(),
            file_type.is_file(),
            file_type.is_symlink(),
        ) {
            DIRECTORY => {
                let dir_content = path
                    .read_dir()
                    .map_err(|error| IoError(error).to_error(context.stack_trace))?;
                for entry in dir_content {
                    let entry =
                        entry.map_err(|error| IoError(error).to_error(context.stack_trace))?;

                    let path = entry.path();
                    hash_filesystem_branch(context, digest, buffer, predicate, path)?;
                }
                Ok(())
            }
            FILE => {
                let mut file = File::open(path)
                    .map_err(|error| IoError(error).to_error(context.stack_trace))?;

                loop {
                    let bytes_read = file
                        .read(buffer)
                        .map_err(|error| IoError(error).to_error(context.stack_trace))?;

                    if bytes_read > 0 {
                        let buffer = &buffer[..bytes_read];
                        digest.update(buffer);
                    } else {
                        // That's the end of the file.
                        break Ok(());
                    }
                }
            }
            SYMLINK => {
                let sympath = path
                    .read_link()
                    .map_err(|error| IoError(error).to_error(context.stack_trace))?;

                // Invalid path encodings may cause inaccurate hashes.
                if let Some(string_rep) = sympath.to_str() {
                    let bytes = string_rep.as_bytes();
                    digest.update(bytes);
                    Ok(())
                } else {
                    // That symlink could be an attempt at bypassing the signature check.
                    // Refuse it.
                    Err(GenericFailure(
                        format!("{path:?} is an invalid symlink to {sympath:?}").into(),
                    )
                    .to_error(context.stack_trace))
                }
            }
            // No idea what this is. We can't hash it, so consider it something malicious.
            ty => Err(
                GenericFailure(format!("{path:?} has an invalid file type: {ty:?}").into())
                    .to_error(context.stack_trace),
            ),
        }
    } else {
        // We quietly ignore this file.
        Ok(())
    }
}

fn git_repo_predicate(path: &Path) -> bool {
    path != Path::new(".git")
}

#[derive(Debug, Hash, Serialize, Deserialize, Clone)]
enum GitReference {
    Commit(String),
    Branch(String),
    Default,
}

impl GitReference {
    fn as_str(&self) -> &str {
        match self {
            GitReference::Commit(hash) => hash.as_str(),
            GitReference::Branch(name) => name.as_str(),
            GitReference::Default => "HEAD",
        }
    }

    #[cfg(test)]
    fn fetch_head_commit_using_command(
        &self,
        context: &ExecutionContext,
        url: &GitUrl,
    ) -> ExpressionResult<String> {
        if let Self::Commit(commit) = self {
            return Ok(commit.clone());
        }

        use std::process::Command;

        let output = Command::new("git")
            .arg("ls-remote")
            .arg(format!("{url}"))
            .arg(self.as_str())
            .output()
            .map_err(|error| {
                GenericFailure(format!("Failed to run git: {error}").into())
                    .to_error(context.stack_trace)
            })?;

        if !output.status.success() {
            return Err(
                GenericFailure("Git returned failure status".into()).to_error(context.stack_trace)
            );
        }

        match String::from_utf8(output.stderr) {
            Ok(stderr) => {
                if !stderr.is_empty() {
                    context.log.push_message(crate::LogMessage {
                        origin: context.stack_trace.bottom().clone(),
                        level: crate::LogLevel::Warning,
                        message: format!(
                            "Git had stderr output while fetching remote head: {stderr}"
                        )
                        .into(),
                    });
                }
            }
            Err(error) => context.log.push_message(crate::LogMessage {
                origin: context.stack_trace.bottom().clone(),
                level: crate::LogLevel::Warning,
                message: format!("Git stderr output was not UTF8 encoded: {error}").into(),
            }),
        }

        match String::from_utf8(output.stdout) {
            Ok(stdout) => {
                if let Some(hash) = stdout.split_whitespace().next() {
                    Ok(hash.to_string())
                } else {
                    Err(
                        GenericFailure("Failed to parse commit hash from git output".into())
                            .to_error(context.stack_trace),
                    )
                }
            }
            Err(error) => Err(GenericFailure(
                format!("Git stdout output was not UTF8 encoded: {error}").into(),
            )
            .to_error(context.stack_trace)),
        }
    }

    fn fetch_head_commit(
        &self,
        context: &ExecutionContext,
        url: &GitUrl,
    ) -> ExpressionResult<String> {
        if let Self::Commit(commit) = self {
            return Ok(commit.clone());
        }

        let mut remote = Remote::create_detached(format!("{url}"))
            .map_err(|error| GitError(error).to_error(context.stack_trace))?;
        let mut callbacks = RemoteCallbacks::new();
        callbacks.credentials(credential_callback);

        remote
            .connect_auth(Direction::Fetch, Some(callbacks), None)
            .map_err(|error| GitError(error).to_error(context.stack_trace))?;

        let heads = remote.list();

        let name: Cow<str> = match self {
            GitReference::Commit(hash) => hash.into(),
            GitReference::Branch(name) => format!("refs/heads/{name}").into(),
            GitReference::Default => "HEAD".into(),
        };

        if let Some(head) = heads
            .map_err(|error| GitError(error).to_error(context.stack_trace))?
            .iter()
            .find(|remote| remote.name() == name)
        {
            Ok(hex::encode(head.oid()))
        } else {
            Err(
                GenericFailure(format!("Could not find head commit for `{name}`").into())
                    .to_error(context.stack_trace),
            )
        }
    }
}

/// Fetches the user's ssh credentials.
fn credential_callback(
    _url: &str,
    username_from_url: Option<&str>,
    _allowed_types: CredentialType,
) -> Result<Cred, git2::Error> {
    // TODO should we support more ways to override/extend this? Maybe through environment
    // variables?
    // TODO should we only be returning this if `_allowed_types` says ssh is permitted?
    Cred::ssh_key(
        username_from_url.unwrap(),
        None,
        std::path::Path::new(&format!("{}/.ssh/id_rsa", std::env::var("HOME").unwrap())),
        None,
    )
}

#[derive(Debug)]
pub struct GitError(pub git2::Error);

impl ErrorType for GitError {}

impl std::fmt::Display for GitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod test {
    use crate::{compile::full_compile, execution::test_context};

    use super::*;

    const TEST_REPO_URL: &str = "git@github.com:IamTheCarl/CommandCAD.git";
    const TEST_COMMIT: &str = "d5790f6f8a083d308404286eb6635f8c0ea24d9e";

    #[test]
    fn fetch_head_default() {
        test_context([], |context| {
            let url = GitUrl::parse(TEST_REPO_URL).unwrap();

            let reference = GitReference::Default;
            let by_command = reference
                .fetch_head_commit_using_command(&context, &url)
                .unwrap();

            let by_library = reference.fetch_head_commit(&context, &url).unwrap();

            assert_eq!(by_command, by_library);
        })
    }

    #[test]
    fn fetch_head_branch() {
        test_context([], |context| {
            let url = GitUrl::parse(TEST_REPO_URL).unwrap();

            let reference = GitReference::Branch("master".into());
            let by_command = reference
                .fetch_head_commit_using_command(&context, &url)
                .unwrap();

            let by_library = reference.fetch_head_commit(&context, &url).unwrap();

            assert_eq!(by_command, by_library);
        })
    }

    #[test]
    fn fetch_head_commit() {
        test_context([], |context| {
            let url = GitUrl::parse(TEST_REPO_URL).unwrap();

            let reference = GitReference::Commit(TEST_COMMIT.into());
            let by_library = reference.fetch_head_commit(&context, &url).unwrap();

            assert_eq!(TEST_COMMIT, by_library);
        })
    }

    fn test_git_source_node_locking(reference: GitReference) {
        test_context([], |context| {
            let url = GitUrl::parse(TEST_REPO_URL).unwrap();

            let expected_commit = reference
                .fetch_head_commit_using_command(&context, &url)
                .unwrap();

            let input_node = NodeSource::Git {
                remote: TEST_REPO_URL.to_string(),
                reference,
            };

            let repo = input_node.lock(&context).unwrap();
            let (_remote, commit) = repo.as_git().unwrap();

            assert_eq!(expected_commit, *commit);
        })
    }

    #[test]
    fn lock_node_git_source_default() {
        test_git_source_node_locking(GitReference::Default);
    }

    #[test]
    fn lock_node_git_source_branch() {
        test_git_source_node_locking(GitReference::Branch("master".into()));
    }

    #[test]
    fn lock_node_git_source_commit() {
        test_git_source_node_locking(GitReference::Commit(TEST_COMMIT.into()));
    }

    fn get_repo_commit(repo: &Path) -> String {
        use std::process::Command;

        let output = Command::new("git")
            .arg("rev-parse")
            .arg("HEAD")
            .current_dir(repo)
            .output()
            .unwrap();

        assert!(output.status.success());
        let stderr = String::from_utf8(output.stderr).unwrap();
        if !stderr.is_empty() {
            eprintln!("{}", stderr);
        }

        let stdout = String::from_utf8(output.stdout).unwrap();

        stdout
    }

    // #[test]
    // fn update_flake() {
    //     test_context([], |context| {
    //         let root = full_compile(format!("std.Flake(inputs = (input = (url = \"{TEST_REPO_URL}\", commit = \"{TEST_COMMIT}\")), output = (input: std.types.Dictionary) -> std.types.Dictionary: input)"));

    //         todo!()
    //     })
    // }
}
