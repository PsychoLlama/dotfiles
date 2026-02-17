---
description: ALWAYS use this skill for anything related to Nix, NixOS, nix-darwin, home-manager, or nixpkgs. This includes writing Nix code, looking up modules/options/packages, answering questions, and debugging Nix errors.
---

- Use `nix eval` and `nix build` to experiment with your changes.
- New files are not discoverable by Nix until you `git add` them.
- Prefer `lib` functions over `builtins` when both exist (e.g. `lib.map` over `builtins.map`).
- Don't guess hostnames or other configuration names. Use `nix flake show` or `nix repl` to discover them.
- If you delegate Nix-related work to a subagent, always instruct it to invoke the `using-nix` skill so it has the same context you do.

# Exploring flake outputs

- Use `nix flake show` to see what a flake exposes.
- Pipe expressions to `nix repl .#` for deeper exploration. Flake outputs are loaded as top-level variables (e.g. `echo 'builtins.attrNames nixosConfigurations' | nix repl .#`).

# Looking up documentation

- Use `man configuration.nix` to explore available module options. This returns NixOS options on Linux and nix-darwin options on macOS.
- Use `man home-configuration.nix` to explore home-manager module options.
- Use `echo ':doc lib.mkIf' | nix repl nixpkgs` to check documentation on `lib` functions. Many include type signatures and examples.

# Source-diving

- Use `EDITOR=cat nix edit nixpkgs#<package>` to view a package's source. Does not work with modules.
- Use `nix flake metadata <flake> --json | jq -r '.path'` to get a flake's store path, then read files directly from it to source-dive modules and other definitions.
- NEVER run `find`, `fd`, `glob`, `grep`, or any broad search against `/nix/store`. It contains millions of files and will hang or take an unreasonable amount of time. Always resolve the exact store path you need first (e.g. via `nix flake metadata` or `nix build --print-out-paths`), then read from that path directly.
