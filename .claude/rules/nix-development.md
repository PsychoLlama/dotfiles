---
paths:
  - "**/*.nix"
---

## Style

- Namespace new options like `foo.bar`, not `fooBar`.
- Use dotted syntax for single-field attrsets (`foo.bar = "baz";`). Expand into a nested block when there are 2+ fields.
- Always declare `options` with block syntax, even when `enable` is the only field.
- Prefer `pkgs.writeShellApplication` for shell scripts.

## Testing

Validate your change before committing.

- `nix eval`: Light. Appropriate for config changes, refactors, new modules, etc.
- `nix build`: Light. Appropriate for package updates and heavier refactors.
- `just build`: Heavy. Appropriate for flake updates and large cross-cutting changes.
