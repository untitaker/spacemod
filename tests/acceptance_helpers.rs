use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

use anyhow::Error;
use tempfile::TempDir;

pub struct Harness {
    pub project_root: Option<TempDir>,
    pub cwd: PathBuf,
    pub binary: PathBuf,
}

impl Drop for Harness {
    fn drop(&mut self) {
        if std::env::var("SPACEMOD_TEST_LEAK_FILES").unwrap_or_default() == "1" {
            std::mem::forget(self.project_root.take());
        }
    }
}

impl Harness {
    pub fn join(&self, path: impl AsRef<Path>) -> PathBuf {
        self.cwd.join(path)
    }
}

pub fn setup() -> Result<Harness, Error> {
    let binary = current_dir()?.join("target/debug/spacemod").to_owned();
    let tempdir = tempfile::tempdir()?;
    Ok(Harness {
        cwd: tempdir.path().to_owned(),
        project_root: Some(tempdir),
        binary,
    })
}

#[allow(unused_macros)]
macro_rules! assert_cmd {
    ($harness:expr, spacemod $($arg:literal)*, $($insta_args:tt)*) => {{
        use std::process::Command;

        insta_cmd::assert_cmd_snapshot!(
            Command::new(&$harness.binary)
            .current_dir(&$harness.cwd)
            $(.arg($arg))*,
            $($insta_args)*
        );
    }}
}

#[allow(unused_imports)]
pub(crate) use assert_cmd;
