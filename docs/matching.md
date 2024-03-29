# Matching modes

By default, your pattern is interpreted as a regular RE2-style regex using
[regex](https://docs.rs/regex/) crate.

`spacemod` provides two flags to change that:

* `-S` to use parenthesis-matching and implicit whitespace, a.k.a "space mode".
* `-F` to interpret the pattern as a literal string to find, same as `-F` in
  fastmod.

`-S` requires further explanation. Let's motivate it with an example:

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
spacemod -S '" (.*) " \.to_string ( )' 'String::from("$1")'
```

The correct end result looks like:

```rust
vec![String::from("foo"), String::from("bar"), String::from("baz")]
```

Spacing out parenthesis and quotes tells spacemod that those tokens, `""` and `()`:

* must be balanced wherever they appear in a match
* can have surrounding whitespace (including newlines)
* are literals (fewer backslashes!)

As a result of implicit whitespace, spacemod would have also found all matches
in the following code:

```rust
"
foo".to_string(
)

"foo"
    .to_string()
```

## Syntax reference

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
