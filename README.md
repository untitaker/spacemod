<div class="oranda-hide">

# spacemod

</div>

`spacemod` is a text search-and-replace tool optimized towards refactoring
code.

```python
# example.py (before)
copy_file(to_file=to_file, from_file=from_file)

copy_file(
    to_file=get_file(filepath, mode),
    from_file=get_file_writer(other_filepath, other_mode)
)
```

```sh
# Use spacemod's custom pattern-matching language to deal with whitespace easier.
# Without -S, normal regex patterns are assumed.

$ spacemod -S \
  'copy_file ( to_file= (.*) , from_file= (.*) )' \
  'copy_file($2, $1)' \
  example.py

# spacemod will open interactive TUI and ask for approval of diffs. Use
# --accept-all to use spacemod non-interactively.
```

```python
# example.py (after)
copy_file(from_file, to_file)

copy_file(get_file_writer(other_filepath, other_mode)
, get_file(filepath, mode))
```

It is very similar to [fastmod](https://github.com/facebookincubator/fastmod),
but with some additional features:

* **Undo stack.** Approved a diff too soon? Hit `[u]ndo` to revert.
* **Yes to all diffs like this.** Auto-approve future diffs with the exact same content.
* **Parenthesis-matching (experimental).** Besides regex, spacemod also
  supports a custom regex-like language that requires less escaping and
  whitespace-handling.
* **Replace recently edited files first.** Spacemod tries to show files with
  higher mtime first, because they are more likely to be relevant to what you
  are working on.

<!-- oranda already provides installation instructions -->
<div class="oranda-hide">

## Documentation

Check [the website](https://untitaker.github.io/spacemod/) for installation and usage.

## License

Licensed under `MIT`, see `./LICENSE`.

</div>
