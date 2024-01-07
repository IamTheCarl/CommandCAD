use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Result};
use semver::Version;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct PackageManifest {
    name: String,
    #[serde(default)]
    authors: Vec<String>,
    version: Version,
    edition: Edition,
}

impl PackageManifest {
    pub fn create_new(name: String, path: impl Into<PathBuf>) -> Result<()> {
        let path = path.into();

        if path.exists() {
            bail!("Target directory for package already exists");
        }

        fs::create_dir_all(&path).context("Failed to create project directory")?;

        let authors = get_author().map(|author| vec![author]).unwrap_or_default();

        let package = Self {
            name,
            authors,
            version: Version::new(0, 1, 0),
            edition: Edition::default(),
        };

        let package =
            serde_yaml::to_string(&package).context("Failed to serialize package manifest")?;

        let manifest_path = path.join("manifest.yaml");
        fs::write(&manifest_path, package).context("Failed to write project manifest.")?;

        if let Err(error) = init_git(&path, &manifest_path) {
            log::warn!("Failed to initalize project as git repository: {:?}", error);
        }

        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Edition {
    #[serde(rename = "2024")]
    E2024,
}

impl Default for Edition {
    fn default() -> Self {
        Self::E2024
    }
}

fn init_git(path: &Path, manifest_path: &Path) -> Result<()> {
    let repository = git2::Repository::init(path).context("Failed to initalize")?;
    let mut index = repository.index().context("Failed to get index")?;
    index
        .add_path(manifest_path)
        .context("Failed to add manifest to index.")?;

    Ok(())
}

/// Attempts to get the name of the developer authoring this package.
fn get_author() -> Option<String> {
    match get_author_from_git() {
        Ok(author) => Some(author),
        Err(error) => {
            log::warn!("Failed to get author name from git: {:?}", error);
            log::info!("Will fallback to using system user's name instead");
            Some(get_author_from_username())
        }
    }
}

fn get_author_from_username() -> String {
    whoami::realname()
}

fn get_author_from_git() -> Result<String> {
    use git2::Config;

    let git_config = Config::open_default().context("Failed to open git config")?;
    let name = git_config.get_string("user.name").ok();
    let email = git_config.get_string("user.email").ok();

    match (name, email) {
        (Some(name), Some(email)) => Ok(format!("{}: {}", name, email)),
        (Some(name), None) => {
            log::warn!("Designer email not set in git config");
            Ok(name)
        }
        (None, Some(email)) => {
            log::warn!("Designer name not set in git config");
            Ok(email)
        }
        _ => bail!("Git configuration did not contain a name or email"),
    }
}
