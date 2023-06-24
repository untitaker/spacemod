mod expr;

use std::collections::VecDeque;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Error};
use console::{style, Key};
use structopt::StructOpt;
use thiserror::Error;

use expr::{parse_pairs, Expr};

#[derive(StructOpt)]
#[structopt(name = "spacemod")]
struct Cli {
    /// The pattern to search for.
    search: String,
    /// The string to replace it with.
    ///
    /// Capture groups such as `(.*)` can be addressed with $0, $1,
    /// $2, ...  where $0 refers to the entire match.
    replace: String,

    /// Optionally, a file or directory to modify instead of `./`
    file_or_dir: Vec<PathBuf>,

    /// Have regex work over multiple lines.
    ///
    /// When using parenthesis-matching, multiline mode is already enabled.
    #[structopt(short = "m", long = "multiline")]
    multiline: bool,
    /// Automatically accept all changes (use with caution).
    #[structopt(long = "accept-all")]
    accept_all: bool,
    /// A list of file extensions to process. Either comma-delimited or by passing the option
    /// multiple times.
    #[structopt(short = "e", long = "extensions", use_delimiter(true))]
    extensions: Vec<String>,
    /// A set of parenthesis to support in addition to the defaults. This option is necessary for
    /// spacemod to understand that ')' is the counterpart to '(', for example.
    ///
    /// By default the value is:
    ///
    ///     -p "{ }"
    ///     -p "( )"
    ///     -p "[ ]"
    ///     -p "< >"
    ///     -p "' '"
    ///     -p '` `'
    ///     -p '" "'
    ///
    /// Specifying this option will append to that list. The option can be specified multiple
    /// times.
    #[structopt(short = "p", long = "pairs")]
    pairs: Vec<String>,
}

#[derive(Copy, Clone)]
enum PromptAnswer {
    Yes,
    No,
    Undo,
    All,
}

#[derive(Error, Debug)]
#[error("invalid answer!")]
struct PromptError;

fn main() -> Result<(), Error> {
    let Cli {
        search,
        replace,
        mut accept_all,
        extensions,
        file_or_dir,
        multiline,
        pairs,
    } = Cli::from_args();
    let term = console::Term::stdout();

    let user_defined_pairs = parse_pairs(&pairs)?;
    let expr = Expr::parse_expr(&search, user_defined_pairs.clone())
        .context("failed to parse search string")?;
    let replacer = expr.get_replacer(multiline, user_defined_pairs)?;

    let mut undo_stack = VecDeque::new();

    let mut walk_builder = if file_or_dir.is_empty() {
        ignore::WalkBuilder::new(".")
    } else {
        let mut walk_builder = ignore::WalkBuilder::new(&file_or_dir[0]);

        for file in &file_or_dir[1..] {
            walk_builder.add(file);
        }

        walk_builder
    };

    if !extensions.is_empty() {
        let mut builder = ignore::overrides::OverrideBuilder::new(".");
        for ext in extensions {
            builder.add(&format!("*.{}", ext))?;
        }

        walk_builder.overrides(builder.build()?);
    }

    let walker = walk_builder.build().filter_map(|entry| {
        let entry = match entry {
            Ok(x) => x,
            Err(e) => return Some(Err(e)),
        };

        let filetype = entry.file_type().unwrap();
        if !filetype.is_file() {
            return None;
        }
        let file_path = entry.path().to_owned();
        let file = match fs::read_to_string(&file_path) {
            Ok(x) => x,
            Err(_) => return None, // presumably binary file
        };

        Some(Ok((file_path, file)))
    });
    let mut walker = itertools::put_back_n(walker);

    'files: while let Some(result) = walker.next() {
        let (mut file_path, mut file) = result?;

        let mut flashed_message = None;

        let mut changed = false;

        let mut change_i = 0;

        let change_file = |file_path: &Path, file: &mut String, to: String| -> Result<(), Error> {
            if fs::read_to_string(file_path).map_or(true, |x| x != *file) {
                anyhow::bail!("file was changed while spacemod is running. Bailing out!");
            }
            *file = to;
            fs::write(file_path, &*file)?;
            Ok(())
        };

        loop {
            let new_file = replacer.replace(&file, &replace);

            if new_file == file {
                break;
            }

            if !accept_all {
                term.clear_screen()?;

                let changeset = difference::Changeset::new(&file.to_string(), &new_file, "\n");
                print_changeset(changeset);
                println!(
                    "\n\n\n{path} [{change_i}] {flashed_message}",
                    path = style(file_path.display()).blue(),
                    change_i = style(change_i).yellow(),
                    flashed_message = style(flashed_message.take().unwrap_or_default()).red()
                );

                println!(
                    "Accept changes? {y} {n} [u]ndo [A]ll",
                    y = style("[y]es").green(),
                    n = style("[n]o").red(),
                );

                let input = loop {
                    match term.read_key()? {
                        Key::Char('y') | Key::Enter => break PromptAnswer::Yes,
                        Key::Char('n') => break PromptAnswer::No,
                        Key::Char('u') => break PromptAnswer::Undo,
                        Key::Char('A') => break PromptAnswer::All,
                        _ => continue,
                    }
                };

                match input {
                    PromptAnswer::No => {
                        // BUG: this skips over the entire file instead of skipping a single change
                        continue 'files;
                    }
                    PromptAnswer::All => {
                        accept_all = true;
                    }
                    PromptAnswer::Undo => {
                        if let Some((old_file_path, change_from, change_to)) =
                            undo_stack.pop_front()
                        {
                            if old_file_path != file_path {
                                walker.put_back(Ok((file_path, file.clone())));
                            }
                            file_path = old_file_path;
                            file = change_from;
                            change_file(&file_path, &mut file, change_to)?;
                            change_i -= 1;
                            continue;
                        } else {
                            flashed_message = Some("!!! nothing on the undo stack");
                            continue;
                        }
                    }
                    PromptAnswer::Yes => {}
                }
            }

            changed = true;

            undo_stack.push_front((file_path.clone(), new_file.to_string(), file.to_string()));
            undo_stack.truncate(1024);

            let new_file = new_file.to_string();
            change_file(&file_path, &mut file, new_file)?;
            change_i += 1;
        }

        if accept_all && changed {
            println!("changed {}", file_path.display());
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
