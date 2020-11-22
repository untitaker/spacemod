# spacemod

`spacemod` is a text search-and-replace tool optimized towards refactoring code.
It is very similar to [fastmod](https://github.com/facebookincubator/fastmod)
but extends the regex syntax with support for matching parenthesized
expressions.

`spacemod` ideally allows you to write more naive regexes and worry less about
overzealous wildcard matching.

***`spacemod` is still alpha software and full of known bugs.***

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
* are literals (less backslashes!)

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
`\ `. That also means escaping a regex like `\ ` as `\\ `.

`spacemod` hardcodes support for the following parenthesis/quotes:

* `{}`
* `[]`
* `()`
* `""` and `''`. Since start and end are the same, this basically
  devolves into asserting there is an even amount of tokens.

Currently this list cannot be extended, but perhaps in the future.

Take a look at [the grammar definition](src/expr.pest) for detailed information.

## License

Licensed under `MIT`, see [`./LICENSE`](./LICENSE).
