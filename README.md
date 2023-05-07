# spacemod

`spacemod` is a text search-and-replace tool optimized towards refactoring code.
It is very similar to [fastmod](https://github.com/facebookincubator/fastmod)
but extends the regex syntax with support for matching parenthesized
expressions.

`spacemod` ideally allows you to write more naive regexes and worry less about
overzealous wildcard matching.

***`spacemod` is still alpha software.*** Please [report](https://github.com/untitaker/spacemod/issues) any bugs you find.

## Example

Let's say you have the following piece of code:

```rust
vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]
```

You get the terrible idea of changing all of your `x.to_string()` calls to
`String::from(x)` instead.

In `fastmod`, or any regex-based search-and-replace, you'd write:

```bash
fastmod '"(.*)"\.to_string\(\)' 'String::from("$1")'
```

You forgot about the greediness of `.*` and should've maybe written `.*?`
instead. Now your code looks like:

```rust
vec![String::from(String::from(String::from("foo"), "bar"), "baz")]
```

Let's try that again. `spacemod` lets you write:

```bash
spacemod '" (.*) " \.to_string ( )' 'String::from("$1")'
```

The correct end result looks like:

```rust
vec![String::from("foo"), String::from("bar"), String::from("baz")]
```

Spacing out parenthesis and quotes tells spacemod that those tokens, `""` and `()`:

* must be balanced wherever they appear in a match
* can have surrounding whitespace (including newlines)
* are literals (fewer backslashes!)

## Installation

Check out this repository, [install Rust](https://rustup.rs/), and run:

```bash
cargo build --release
```

Your binary is in `./target/release/spacemod`.

## Parenthesis matching

`spacemod`'s pattern syntax consists of tokens delimited by a single space each.
A token can either be a parenthesis/quote, or a substring of a regex:

```
{ regex1 } regex2 { regex3 }
```

If you need to use a literal space anywhere in a regex substring, escape it as
`\ `. That also means escaping a regex like `\ ` as `\\ `. Backslashes followed
by anything but a space within a regex do not need to be escaped.

`spacemod` knows the following parenthesis/quotes:

* `{}`
* `[]`
* `()`
* `<>`
* `"", ``, ''`. Since start and end are the same, this basically
  devolves into asserting there is an even number of tokens.

You can extend this list with `-p ab` where `a` is the opening parenthesis, and
`b` the closing counterpart. See `--help` for more information.

## Alternatives

You may find the following tools useful if spacemod is doing too much or too
little for you. The primary focus of this list is on editor/IDE-independent
tools, and preferably on those which can be composed into more complex
shell-scripts.

* `spacemod` is heavily inspired by
  [fastmod](https://github.com/facebookincubator/fastmod) and was specifically
  built to deal with shortcomings of regex. `fastmod` is much faster.

* [`comby`](https://comby.dev/) actually has grammars built-in for various
  filetypes to understand what wildcards are supposed to match in which
  contexts. It appears to be "a step up" from `spacemod` the same way
  `spacemod` syntax is a step up from regex. That goes for both expressiveness
  and complexity.

* [`codemod`](https://github.com/facebook/codemod) is another search-and-replace tool that has
  language-specific knowledge. It supports both basic regex replacements and
  more sophisticated transformations written in Python.

* [`spatch`](https://github.com/facebookarchive/pfff/wiki/Spatch) from the
  PFFF-suite appears to be very similar to `comby`.

### Other tools in the same space

* [Beyond Grep](https://beyondgrep.com/more-tools/) has a table of
  (regex-based) *text search* tools. The best one IMO is
  [`ripgrep`](https://github.com/BurntSushi/ripgrep).

* [`semgrep`](https://github.com/returntocorp/semgrep) is a search tool with a
  bit of semantic knowledge, but also general text matching abilities that go
  beyond regular expressions.

## License

Licensed under `MIT`, see [`./LICENSE`](./LICENSE).
