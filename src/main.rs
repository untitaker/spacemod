mod expr;

use std::collections::{BTreeSet, VecDeque};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::mpsc::sync_channel;

use anyhow::{Context, Error};
use console::{style, Key, Style};
use ignore::WalkState;
use itertools::Itertools;
use similar::{ChangeTag, TextDiff};
use structopt::StructOpt;

use expr::{parse_pairs, Expr, Replacer};

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

    /// How many threads to use, default is to try and saturate CPU.
    #[structopt(short = "j", long = "jobs")]
    threads: Option<usize>,

    /// Enable replacing in hidden files.
    #[structopt(short = "u", long = "hidden")]
    hidden: bool,

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
    YesDiff,
    NoDiff,
    All,
}

fn main() -> Result<(), Error> {
    let Cli {
        search,
        replace,
        threads,
        hidden,
        accept_all,
        extensions,
        file_or_dir,
        multiline,
        pairs,
    } = Cli::from_args();

    // most of the work we do is kind of I/O bound. rayon assumes CPU-heavy workload. we could
    // look into tokio-uring at some point, but it seems like a hassle wrt ownership
    let threads = threads.unwrap_or_else(|| 4 * num_cpus::get());

    rayon::ThreadPoolBuilder::new()
        .num_threads(threads)
        .build_global()
        .unwrap();

    let user_defined_pairs = parse_pairs(&pairs)?;
    let expr = Expr::parse_expr(&search, user_defined_pairs.clone())
        .context("failed to parse search string")?;
    let replacer = expr.get_replacer(multiline, user_defined_pairs)?;

    let mut walk_builder = if file_or_dir.is_empty() {
        ignore::WalkBuilder::new(".")
    } else {
        // filter out duplicate CLI arguments, because otherwise our entire UI starts misbehaving.
        let mut file_or_dir = file_or_dir.into_iter().unique();
        let mut walk_builder = ignore::WalkBuilder::new(&file_or_dir.next().unwrap());

        for file in file_or_dir {
            walk_builder.add(file);
        }

        walk_builder
    };

    walk_builder.hidden(!hidden);

    if !extensions.is_empty() {
        let mut builder = ignore::overrides::OverrideBuilder::new(".");
        for ext in extensions {
            builder.add(&format!("*.{}", ext))?;
        }

        walk_builder.overrides(builder.build()?);
    }

    let mut result = None;

    rayon::scope(|scope| {
        let (sender, receiver) = sync_channel(128);

        let replacer2 = &replacer;

        scope.spawn(move |_| {
            walk_builder.threads(threads).build_parallel().run(|| {
                let sender = sender.clone();
                Box::new(move |entry_result| {
                    let entry = match entry_result {
                        Ok(x) => x,
                        Err(e) => {
                            sender.send(Err(e.into())).ok();
                            return WalkState::Quit;
                        }
                    };

                    let filetype = entry.file_type().unwrap();
                    if !filetype.is_file() {
                        return WalkState::Continue;
                    }
                    let file_path = entry.path().to_owned();
                    let file = match fs::read_to_string(&file_path) {
                        Ok(x) => x,
                        Err(_) => return WalkState::Continue, // presumably binary file
                    };

                    if !replacer2.prefilter_matches(&file) {
                        return WalkState::Continue;
                    }

                    sender.send(Ok((file_path, file))).ok();
                    WalkState::Continue
                })
            });
        });

        scope.spawn(|_| {
            result = Some(run_ui(
                &replace,
                accept_all,
                &replacer,
                receiver.into_iter(),
            ));
        });
    });

    result.unwrap()
}

fn run_ui(
    replace: &str,
    mut accept_all: bool,
    replacer: &Replacer<'_>,
    receiver: impl Iterator<Item = Result<(PathBuf, String), Error>>,
) -> Result<(), Error> {
    let term = console::Term::stdout();
    let mut walker = itertools::put_back_n(receiver);
    let mut undo_stack = VecDeque::<(PathBuf, String, String)>::new();
    let mut yes_diffs = BTreeSet::new();
    let mut no_diffs = BTreeSet::new();

    'files: while let Some(result) = walker.next() {
        let (mut file_path, mut file) = result?;

        let mut flashed_message = None;

        let mut change_i = 0;

        let change_file =
            |file_path: &Path, change_from: &str, change_to: &str| -> Result<(), Error> {
                if fs::read_to_string(file_path).map_or(true, |x| x != *change_from) {
                    anyhow::bail!(
                        "file {} was changed while spacemod is running. Bailing out!",
                        file_path.display()
                    );
                }
                fs::write(file_path, change_to)?;
                Ok(())
            };

        loop {
            let new_file = replacer.replace(&file, replace);

            if new_file == file {
                continue 'files;
            }

            let changeset_hash = hash_changeset(&file, &new_file);

            let input = if accept_all || yes_diffs.contains(&changeset_hash) {
                println!("Automatically changed {}", file_path.display());
                PromptAnswer::Yes
            } else if no_diffs.contains(&changeset_hash) {
                PromptAnswer::No
            } else {
                term.clear_screen()?;

                print_changeset(&file, &new_file);
                println!(
                    "\n\n\n{path} [{change_i}] {flashed_message}",
                    path = style(file_path.display()).blue(),
                    change_i = style(change_i).yellow(),
                    flashed_message = style(flashed_message.take().unwrap_or_default()).red()
                );

                println!(
                    "Accept changes?\n\
                    {y} [Y]es to all diffs like this (=sharing underlined part)\n\
                    {n}  [N]o to all diffs like this\n\
                    [u]ndo\n\
                    [A]pprove everything",
                    y = style("[y]es").green(),
                    n = style("[n]o").red(),
                );

                loop {
                    match term.read_key()? {
                        Key::Char('y') | Key::Enter => break PromptAnswer::Yes,
                        Key::Char('n') => break PromptAnswer::No,
                        Key::Char('u') => break PromptAnswer::Undo,
                        Key::Char('Y') => break PromptAnswer::YesDiff,
                        Key::Char('N') => break PromptAnswer::NoDiff,
                        Key::Char('A') => break PromptAnswer::All,
                        _ => continue,
                    }
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
                    if let Some((old_file_path, change_from, change_to)) = undo_stack.pop_front() {
                        if old_file_path != file_path {
                            walker.put_back(Ok((file_path, file.clone())));
                        }
                        change_file(&old_file_path, &change_from, &change_to)?;
                        file_path = old_file_path.to_owned();
                        file = change_to.to_owned();
                        change_i -= 1;
                        continue;
                    } else {
                        flashed_message = Some("!!! nothing on the undo stack");
                        continue;
                    }
                }
                PromptAnswer::Yes => {}
                PromptAnswer::YesDiff => {
                    yes_diffs.insert(changeset_hash);
                }
                PromptAnswer::NoDiff => {
                    no_diffs.insert(changeset_hash);
                    continue;
                }
            }

            undo_stack.push_front((file_path.clone(), new_file.to_string(), file.to_string()));
            undo_stack.truncate(1024);

            let new_file = new_file.to_string();
            change_file(&file_path, &file, &new_file)?;
            file = new_file;
            change_i += 1;
        }
    }

    Ok(())
}

/// A function that works around some crappy behavior in the `difference` crate.
///
/// That crate will print out very long files even if there's a minuscle change in it.
fn print_changeset(old: &str, new: &str) {
    struct Line(Option<usize>);

    impl fmt::Display for Line {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.0 {
                None => write!(f, "    "),
                Some(idx) => write!(f, "{:<4}", idx + 1),
            }
        }
    }

    let diff = TextDiff::from_lines(old, new);

    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            println!("{:-^1$}", "-", 80);
        }
        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, s) = match change.tag() {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new().dim()),
                };
                print!(
                    "{}{} |{}",
                    style(Line(change.old_index())).dim(),
                    style(Line(change.new_index())).dim(),
                    s.apply_to(sign).bold(),
                );
                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        print!("{}", s.apply_to(value).underlined().on_black());
                    } else {
                        print!("{}", s.apply_to(value));
                    }
                }
                if change.missing_newline() {
                    println!();
                }
            }
        }
    }
}

fn hash_changeset(old: &str, new: &str) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();

    let diff = TextDiff::from_lines(old, new);

    for group in diff.grouped_ops(3).iter() {
        for op in group {
            for change in diff.iter_inline_changes(op) {
                match change.tag() {
                    ChangeTag::Delete => hasher.update(b"\x00\n"),
                    ChangeTag::Insert => hasher.update(b"\x01\n"),
                    _ => continue,
                };

                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        hasher.update(value.as_bytes());
                    }
                }
            }
        }
    }

    hasher.finalize().as_bytes().to_owned()
}
