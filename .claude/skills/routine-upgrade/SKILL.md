---
description: Perform a routine upgrade.
disable-model-invocation: true
user-invocable: true
---

## Steps

1. Run `just update` to update all flake inputs and custom packages.
2. Validate with `just build`.
3. Fix failures and deprecation warnings.
4. Once everything passes, commit with a changelog.

## Curating a Changelog

- Capture the diff printed by `just build`.
- Summarize meaningful package version changes and deprecations from the diff.
- Do not search the web or dive into source code to identify changes. The output is your source of truth.
