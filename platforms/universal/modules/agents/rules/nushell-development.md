---
paths:
  - "**/*.nu"
---

## Conventions

- Libraries with subcommands: give the module an `export def main [] { help modules <name> }`. The bare command then surfaces its help; real behavior lives in subcommands (`export def 'name action' [...]`).
- Enumerate enum values in doc comments, one line of meaning each. The doc comment above a command renders as its `help <cmd>` description.
- Attach `@example "desc" { ... } --result <value>` attributes where they aid discovery; they render under `help <cmd>`. See `help 'attr example'`.
- Prefer functional pipelines (`each`, `where`, `reduce`) over `mut` + `for`. Exception: a `for` loop keeps a thrown error's span on the offending input, where `each` buries it under an `eval_block_with_input` wrapper.

## Scripting

Standalone invocable scripts use a `nu --stdin` shebang and an `export def main` entrypoint. `--stdin` binds piped input to `$in` inside `main`; mark the file executable.

```nu
#!/usr/bin/env -S nu --stdin

# Prefix each piped line with a label.
export def main [label: string] {
  $in | lines | each { |line| $"($label): ($line)" }
}
```

## Validation

Check your changes with `nu-check`. Must pass before committing.

`nu-check` is a Nushell built-in, not a binary on `PATH`, so run it through `nu -c`:

```bash
nu -c "nu-check --debug '<absolute-path>'"
```

Prints `true` and exits 0 when clean; on a parse error it prints the failure and exits non-zero.
