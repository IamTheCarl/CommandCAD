use std::collections::HashMap;

use anyhow::{Context, Result};

mod parsing;
pub use parsing::Span;

mod module;
use module::Module;
pub use module::ValidationLog;

#[derive(Debug, Default)]
pub struct Runtime<S: Span> {
    modules: HashMap<String, Module<S>>,
    external_modules: HashMap<String, Module<S>>,
}

impl<S: Span> Runtime<S> {
    pub fn load_module(
        &mut self,
        log: &mut ValidationLog<S>,
        name: impl Into<String>,
        file_name: impl Into<String>,
        code: impl Into<S>,
    ) -> Result<()> {
        let name = name.into();
        let file_name = file_name.into();
        let code = code.into();

        let module = Module::load(log, file_name.clone(), code)
            .with_context(|| format!("Failed to load module `{}` from `{}`", name, file_name))?;

        self.modules.insert(name, module);
        Ok(())
    }

    pub fn build_sketch(&mut self, module: &str, name: &str) -> Result<()> {
        todo!()
    }

    pub fn build_widget(&mut self, module: &str, name: &str) -> Result<()> {
        todo!()
    }

    pub fn run_function(&mut self, module: &str, name: &str) -> Result<()> {
        todo!()
    }
}
