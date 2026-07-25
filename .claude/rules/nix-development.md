---
paths:
  - "**/*.nix"
---

## Testing

Validate your change before committing.

- `nix eval`: Light. Appropriate for config changes, refactors, new modules, etc.
- `nix build`: Light. Appropriate for package updates and heavier refactors.
- `just build`: Heavy. Appropriate for flake updates and large cross-cutting changes.
