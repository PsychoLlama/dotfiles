---
description: ALWAYS use this skill for anything related to Nix, NixOS, home-manager, or nixpkgs. This includes writing Nix code, looking up modules/options/packages, answering questions, and debugging Nix errors.
---

- Use `nix eval` and `nix build` to experiment with your changes.
- New files are not discoverable by Nix until you `git add` them.
- Prefer `lib` functions over `builtins` when both exist (e.g. `lib.map` over `builtins.map`).
- Don't guess hostnames or other configuration names. Use `nix flake show` or `nix repl` to discover them.
- If you delegate Nix-related work to a subagent, always instruct it to invoke the `using-nix` skill so it has the same context you do.
- NEVER run `find`, `fd`, `glob`, `grep`, or any broad search against `/nix/store`. It's huge. It will cripple the machine. Instead, resolve store paths first.

## Inspecting

- Use `echo ':doc lib.mkIf' | nix repl nixpkgs` to see function usage and type signatures.
- Use `nix flake show --json` to see flake exports.
- Use `EDITOR=cat nix edit nixpkgs#<package>` to view a package's source. Does not work with modules.
- Resolve store paths with `nix flake archive --json [<flake>]` (defaults to `.`): the flake itself is `.path`, a pinned input is `.inputs["<name>"].path`, and transitive inputs nest under `.inputs.<name>.inputs.<child>.path`.
- Query outputs with `nix eval` (`--apply` to transform, `--json` for clean output):

```sh
# List an output's attribute names
nix eval .#nixosConfigurations --apply builtins.attrNames --json

# Read a nested value directly by attribute path
nix eval .#nixosConfigurations.<host>.config.networking.hostName

# Evaluate an arbitrary expression (--impure unlocks getFlake, <nixpkgs>, getEnv)
nix eval --impure --expr '(builtins.getFlake (toString ./.)).inputs.nixpkgs.lib.versions.major "1.2.3"'
```
