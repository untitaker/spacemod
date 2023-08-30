
# Alternatives

You may find the following tools useful if spacemod is doing too much or too
little for you. The primary focus of this list is on editor/IDE-independent
tools, and preferably on those which can be composed into more complex
shell-scripts.

* `spacemod` is heavily inspired by
  [fastmod](https://github.com/facebookincubator/fastmod) and was specifically
  built to deal with shortcomings of regex. `fastmod` is much faster.

* [`ast-grep`](https://ast-grep.github.io/) is a very easy to use AST-based
  code search tool.

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

## Other tools in the same space

* [Beyond Grep](https://beyondgrep.com/more-tools/) has a table of
  (regex-based) *text search* tools. The best one IMO is
  [`ripgrep`](https://github.com/BurntSushi/ripgrep).
