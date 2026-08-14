---
description: Use when the user mentions swizzling, asks you to swizzle a config file, or when iterating on a Nix-managed config in place before porting the change back into Nix.
---

- The user may have swizzled a file or suggest you swizzle it.
- "Swizzling" refers to moving the nix-managed file to a `.bak` and symlinking a program's config to a file copied into the local directory.
- It's a custom nushell command.
- Swizzled files must never be committed. Changes should be adapted into their nix-managed equivalent.

Swizzling a file:

```bash
# Drops `config.nu` into the current directory.
${CLAUDE_SKILL_DIR}/nu-run 'swizzle edit ~/.config/nushell/config.nu'
```

If you swizzled the file (not the user), restore it when you're done:

```bash
${CLAUDE_SKILL_DIR}/nu-run 'swizzle revert ~/.config/nushell/config.nu'
```

For usage:

```bash
${CLAUDE_SKILL_DIR}/nu-run 'swizzle <cmd> --help'
```
