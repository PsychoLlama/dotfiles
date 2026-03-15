---
description: Update flake inputs and custom packages, then address deprecations.
disable-model-invocation: true
---

!`just update`

## Steps

1. Run checks.
2. Fix failures and deprecation warnings.
3. Once everything passes, commit with a changelog.

## Curating a Changelog

- Use `dix <old-store-path> <new-store-path>` to diff the system's Nix store paths before and after the update.
- Summarize meaningful package version changes and deprecations from the diff.
- Do not search the web or dive into source code to identify changes. The `dix` output is your source of truth.
