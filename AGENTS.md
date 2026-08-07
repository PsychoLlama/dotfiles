# Dotfiles

NixOS-based configuration-as-code for Linux and home-manager environments.

## Architecture

This flake is consumed by other flakes. Everything must be changeable, disableable, or extendable from the outside.

Every `.nix` file under `modules/` is a flake module (the dendritic pattern). Files are organized by concern, not by module class, so one file holds every class a concern touches — `modules/programs/sway.nix` configures both NixOS and Home Manager.

Each file exports into `flake.modules.<class>.default`, typed as a `deferredModule`, so contributions from every file merge into one module per class. `lib/hosts` imports those into each substrate. Classes are `nixos`, `homeManager`, and `editor`; `generic` declares no class and loads into all three.

Two kinds of module share the tree:

- **Platform extensions** (`modules/extensions/`) — new programs, services, and DSLs. Keep opinions out; these should be upstreamable. They only declare options, so they export unconditionally.
- **Presets** — opinionated configs under the `psychollama.*` namespace, gated on their own `enable`.

Hosts (`hosts/`) hold machine-specific settings only (hardware, disk, display). All generalizable config belongs in presets.

## Directory Structure

- `hosts/` — Machine-specific configs.
- `modules/` — All Nix modules, one directory per concern. `flake.nix` holds inputs only.
  - `flake/` — the flake's own outputs, one file per concern (lib, modules, nixpkgs, hosts, packages, shell, overlays, templates).
  - `extensions/{programs,services}/` — platform extensions.
  - `programs/`, `services/` — presets, one file (or directory) per program or service.
  - `system/` — presets and options belonging to no single program (`fonts`, `gtk`, `identity`, `theme`, `agents`).
  - `profiles/` — groupings of presets.
  - `editor/` — Self-contained neovim framework (see [Editor](#editor)).
- `lib/` — Nix utilities (system builders, module discovery).
- `pkgs/` — Custom package derivations.

Module options mirror the directory structure: `psychollama.presets.programs.foo` lives at `programs/foo.nix` (or `foo/default.nix`). A module keeps a directory when it references sibling assets relatively (`waybar/waybar.css`, `nushell/libraries/`).

## Conventions

### Platform Extensions

- Prefer upstream `home-manager`/`nixos` options. Only add custom modules when upstream lacks support.
- Prefer `home-manager` over per-OS modules; it's the most cross-platform option.
- `_make-program-module.nix` and `_mk-unstable-preset.nix` cover programs whose only options are enable and package. A program needing more imports the helper alongside its own config, keeping settings and pin in one file.

### Presets

- Single-responsibility, `enable` option only.
- Install programs via `programs.<name>.enable` + `programs.<name>.package`, not `home.packages`.
- Reference other programs through their `programs.<name>.package` rather than bare `pkgs.<name>`. Presets often pin `pkgs.unstable.*`, so direct references risk installing both versions.
- Resolve executable paths with `lib.getExe` (single main binary) or `lib.getExe'` (explicit binary name); bind in `let` at top of file.

## Editor

Self-contained neovim framework in `modules/editor/`. No `~/.config` files.

Its vocabulary is plugins and LSP servers rather than programs and services, so it keeps its own tree, laid out on the same convention as the root.

- `platform/` — plugin system, LSP configuration, settings schema. Named for what it is: the editor module class is invented here, so there is no upstream to extend.
- `plugins/`, `lsp/` — presets. `profiles/` — groupings.
- `runtime/lua/core/` — Lua framework for Nix integration (package loading, deferred plugins, settings, LSP).
- `pkgs/dotfiles.nvim/` — neovim utilities beyond `init.vim`.

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
- Every `.nix` file under `modules/` is a flake module. Just drop the file in; `modules/default.nix` import-trees each directory.
- A module reaches a host by exporting to `flake.modules.<class>.default`. Declare its `enable` inside that exported module, not on the flake.
- Helpers, data, and libraries opt out with an `_` prefix (`_auto-format.nix`), which import-tree ignores. `import` them explicitly where needed.
