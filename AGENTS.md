# Dotfiles

NixOS-based configuration-as-code for Linux and home-manager environments.

## Architecture

This flake is consumed by other flakes. Everything must be changeable, disableable, or extendable from the outside.

Each platform exposes two flake-output modules:

- `nixosModules.<platform>-platform` — new programs, services, and DSLs extending the platform. Keep opinions out; these should be upstreamable.
- `nixosModules.<platform>-configs` — opinionated configurations under the `psychollama.*` namespace.

On disk the split is by subdirectory: `platforms/<platform>/modules/psychollama/` is the configs side; everything else under `modules/` is the platform side.

Hosts (`hosts/`) hold machine-specific settings only (hardware, disk, display). All generalizable config belongs in presets.

## Directory Structure

- `hosts/` — Machine-specific configs.
- `platforms/`
  - `editor/` — Self-contained neovim framework (see [Editor](#editor)).
  - `home-manager/` — Home Manager extensions and presets. Platform extensions live under `modules/programs/` and `modules/services/`.
  - `nixos/` — NixOS-only presets and profiles. No standalone platform extensions today.
  - `universal/` — Cross-platform options (`identity`, `theme`) consumed by every system substrate.
- `lib/` — Nix utilities (system builders, module discovery, overlays).
- `pkgs/` — Custom package derivations.

Inside `modules/psychollama/`:

- `presets/` — single-program opinionated configs.
- `profiles/` — groupings of presets.

Module options mirror the directory structure: `psychollama.presets.programs.foo` lives at `psychollama/presets/programs/foo.mod.nix` (or `foo/default.mod.nix`).

## Conventions

### Nix Style

- Namespace new options like `foo.bar`, not `fooBar`.
- Use dotted syntax for single-field attrsets (`foo.bar = "baz";`). Expand into a nested block when there are 2+ fields.
- Always declare `options` with block syntax, even when `enable` is the only field.
- Prefer `pkgs.writeShellApplication` for shell scripts.

### Platform Extensions

- Prefer upstream `home-manager`/`nixos` options. Only add custom modules when upstream lacks support.
- Prefer `home-manager` over per-OS modules; it's the most cross-platform option.
- `makeProgramModule` and `mkUnstablePreset` exist for simple programs (enable + package only). Use standalone files when custom options are needed.

### Presets

- Single-responsibility, `enable` option only.
- Install programs via `programs.<name>.enable` + `programs.<name>.package`, not `home.packages`.
- Reference other programs through their `programs.<name>.package` rather than bare `pkgs.<name>`. Presets often pin `pkgs.unstable.*`, so direct references risk installing both versions.
- Resolve executable paths with `lib.getExe` (single main binary) or `lib.getExe'` (explicit binary name); bind in `let` at top of file.

## Editor

Self-contained neovim framework in `platforms/editor/`. No `~/.config` files.

- `modules/` — plugin system, LSP configuration, settings schema.
- `runtime/lua/core/` — Lua framework for Nix integration (package loading, deferred plugins, settings, LSP).
- `pkgs/dotfiles.nvim/` — neovim utilities beyond `init.vim`.

Plugin presets live under `modules/psychollama/presets/plugins/`; LSP servers under `modules/psychollama/presets/lsp/servers/`.

### Working with Neovim

Always check help pages when working with the neovim API:

```bash
# Find plugin help pages
nvim --headless -c 'help <name> | echo expand("%:p") | qa'

# Find the neovim runtime
nvim --headless -c 'echo $VIMRUNTIME | qa'
```

## Developing

All programs are declaratively managed. When changing configuration for a program (e.g. Claude Code settings, shell aliases, git config), edit the corresponding Nix module — never the dotfiles directly.

- Use `nix eval` to verify settings are applied correctly when refactoring.
- `git add --intent-to-add` new files before Nix can discover them.
- Nix modules in this repo are discovered and imported automatically. No `imports` needed.
- Only `*.mod.nix` files under `platforms/` are imported as modules. Name a module file with the `.mod.nix` suffix (including `default.mod.nix` for directory entrypoints), or it won't be picked up.
- Plain `.nix` files are free to be helpers, data, or libraries — `import` them explicitly where needed.
