use crate::errors::ShellError;
use crate::object::process::Process;
use crate::object::{dir_entry_dict, ShellObject, Value};
use crate::prelude::*;
use crate::Args;
use derive_new::new;
use std::path::{Path, PathBuf};
use sysinfo::SystemExt;

#[derive(new)]
pub struct CdBlueprint;

impl crate::CommandBlueprint for CdBlueprint {
    fn create(
        &self,
        args: Vec<Value>,
        host: &dyn Host,
        env: &mut Environment,
    ) -> Result<Box<dyn Command>, ShellError> {
        let target = match args.first() {
            // TODO: This needs better infra
            None => return Err(ShellError::string(format!("cd must take one arg"))),
            Some(v) => v.as_string()?.clone(),
        };

        Ok(Box::new(Cd {
            cwd: env.cwd().to_path_buf(),
            target,
        }))
    }
}

#[derive(new)]
pub struct Cd {
    cwd: PathBuf,
    target: String,
}

impl crate::Command for Cd {
    fn run(&mut self, stream: VecDeque<Value>) -> Result<VecDeque<ReturnValue>, ShellError> {
        let mut stream = VecDeque::new();
        let path = dunce::canonicalize(self.cwd.join(&self.target).as_path())?;
        stream.push_back(ReturnValue::change_cwd(path));
        Ok(stream)
    }
}