---
name: routine-upgrade
description: Perform a routine upgrade.
disable-model-invocation: true
---

## Steps

1. Run `just update` to update all flake inputs and custom packages.
2. Validate the build (see below).
3. Fix failures and deprecation warnings.
4. Once everything passes, commit with a changelog.

## Building

Never run `just build` directly. Run `.claude/skills/routine-upgrade/launch-build-in-tmux` instead.

- It builds in a new window of the `dotfiles@builds` tmux session and prints the log file path.
- Wait for the log to end with an `exit status: <code>` line.
- Read results from the log. Expect ANSI color codes.
- The window stays open until the user closes it. Don't wait for that.

## Curating a Changelog

- Capture the diff printed by the build log.
- Summarize meaningful package version changes and deprecations from the diff.
- Do not search the web or dive into source code to identify changes. The output is your source of truth.
