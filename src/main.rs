mod expr;

use std::collections::VecDeque;
use std::fmt;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;

use anyhow::{Context, Error};
use thiserror::Error;

use structopt::StructOpt;

use expr::Expr;

#[derive(StructOpt)]
#[structopt(name = "hyperlink")]
struct Cli {
    search: String,
    replace: String,

    file_or_dir: Vec<PathBuf>,

    /// Have regex work over multiple lines.
    ///
    /// When using parenthesis-matching, multiline mode is already enabled.
    #[structopt(short = "m", long = "multiline")]
    multiline: bool,
    /// Automatically accept all changes (use with caution).
    #[structopt(long = "accept-all")]
    accept_all: bool,
    /// A comma-delimited list of file extensions to process.
    #[structopt(short = "e", long = "extensions")]
    extensions: Vec<String>,
}

#[derive(Copy, Clone)]
enum PromptAnswer {
    Yes,
    No,
    All,
}

#[derive(Error, Debug)]
#[error("invalid answer!")]
struct PromptError;

impl FromStr for PromptAnswer {
    type Err = PromptError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "yes" | "y" => Ok(PromptAnswer::Yes),
            "no" | "n" => Ok(PromptAnswer::No),
            "all" | "a" if s == "A" || s.len() != 1 => Ok(PromptAnswer::All),
            _ => Err(PromptError),
        }
    }
}

impl fmt::Display for PromptAnswer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PromptAnswer::Yes => write!(f, "yes"),
            PromptAnswer::No => write!(f, "no"),
            PromptAnswer::All => write!(f, "all"),
        }
    }
}

fn main() -> Result<(), Error> {
    let Cli {
        search,
        replace,
        mut accept_all,
        extensions,
        file_or_dir,
        multiline,
    } = Cli::from_args();
    let term = console::Term::stdout();

    let expr = Expr::parse_expr(&search).context("failed to parse search string")?;
    let replacer = expr.get_replacer(multiline)?;

    let mut walk_builder = ignore::WalkBuilder::new(".");

    for file in file_or_dir {
        walk_builder.add(file);
    }

    if !extensions.is_empty() {
        let mut builder = ignore::overrides::OverrideBuilder::new(".");
        for ext in extensions {
            builder.add(&format!("*.{}", ext))?;
        }

        walk_builder.overrides(builder.build()?);
    }

    'files: for entry in walk_builder.build() {
        let entry = entry?;
        let filetype = entry.file_type().unwrap();
        if !filetype.is_file() {
            continue;
        }

        let mut file = match fs::read_to_string(entry.path()) {
            Ok(x) => x,
            Err(_) => continue, // presumably binary file
        };

        let mut changed = false;

        loop {
            let new_file = replacer.replace(&file, &replace);

            if new_file == file {
                break;
            }

            if !accept_all {
                term.clear_screen()?;

                println!("{}\n\n\n", entry.path().display());
                let changeset = difference::Changeset::new(&file.to_string(), &new_file, "\n");
                print_changeset(changeset);
                print!("\n\n\n");

                let input: PromptAnswer = dialoguer::Input::new()
                    .with_prompt("Accept changes? [y]es [n]o [A]ll")
                    .with_initial_text("")
                    .default(PromptAnswer::Yes)
                    .interact_text()?;

                match input {
                    PromptAnswer::No => {
                        continue 'files;
                    }
                    PromptAnswer::All => {
                        accept_all = true;
                    }
                    PromptAnswer::Yes => {}
                }
            }

            fs::write(entry.path(), &*new_file)?;
            changed = true;

            file = new_file.to_owned().to_string();
        }

        if accept_all && changed {
            println!("changed {}", entry.path().display());
        }
    }

    Ok(())
}

/// A function that works around some crappy behavior in the `difference` crate.
///
/// That crate will print out very long files even if there's a minuscle change in it.
fn print_changeset(mut changeset: difference::Changeset) {
    const MAX_END_PADDING: usize = 5;
    const MAX_START_PADDING: usize = 20;

    use difference::Difference;

    let mut diffs = VecDeque::new();
    let mut encountered_diff = false;
    let mut first_end = None;

    'diff: for diff in changeset.diffs.into_iter() {
        match diff {
            Difference::Add(x) => {
                for line in x.lines() {
                    diffs.push_back(Difference::Add(line.to_owned()));
                    first_end = None;
                    encountered_diff = true;
                }
            }
            Difference::Rem(x) => {
                for line in x.lines() {
                    diffs.push_back(Difference::Rem(line.to_owned()));
                    first_end = None;
                    encountered_diff = true;
                }
            }
            Difference::Same(x) => {
                for line in x.lines() {
                    diffs.push_back(Difference::Same(line.to_owned()));
                    if encountered_diff
                        && *first_end.get_or_insert(diffs.len() - 1) + MAX_END_PADDING < diffs.len()
                    {
                        break 'diff;
                    }
                }
            }
        }

        if !encountered_diff {
            while diffs.len() > MAX_START_PADDING {
                diffs.pop_front();
            }
        }
    }

    changeset.diffs = diffs.into_iter().collect();

    println!("{}", changeset);
}
