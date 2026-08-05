# Dotfiles

NixOS-based configuration-as-code for Linux and home-manager environments.

## Architecture

This flake is consumed by other flakes. Everything must be changeable, disableable, or extendable from the outside.

Each platform exposes two flake-output modules:

- `modules.<class>.platform` — new programs, services, and DSLs extending the platform. Keep opinions out; these should be upstreamable.
- `modules.<class>.configs` — opinionated configurations under the `psychollama.*` namespace.

Classes are `nixos`, `homeManager`, and `editor`. Cross-platform modules live under `generic` (`modules.generic.configs`), which declares no class and imports anywhere. The module system enforces the rest.

On disk the split is by subdirectory: `modules/<class>/psychollama/` is the configs side, `modules/<class>/platform/` is the platform side. A directory is omitted when the class has nothing on that side (`nixos` and `generic` have no platform extensions today).

Host aspects (`modules/aspects/<host>/`) hold machine-specific settings only (hardware, disk, display). All generalizable config belongs in presets.

## Directory Structure

- `modules/` — All Nix modules. `flake.nix` holds inputs only, and `default.nix` lists the flake-module trees.
  - `den.nix` — Den entity data: which hosts exist, and which users live on them. Aspects live elsewhere.
  - `den/` — Den wiring: schema, systems, the `dotfiles` namespace, and `classes/` for custom aspect classes.
  - `aspects/` — Every aspect, entity aspects included. `defaults.nix` defines `den.default`, applied to every entity; `<host>/` and `<user>.nix` hold entity-specific config. Non-flake modules are `_`-prefixed and referenced explicitly.
    - `dotfiles/` — Reusable aspects under the `dotfiles.*` namespace. Split into `programs/`, `services/`, and `profiles/`; anything that is neither a program nor a service sits at the root.
  - `flake/` — the flake's own outputs (packages, shell, overlays, templates).
  - `editor/` — Self-contained neovim framework (see [Editor](#editor)).
  - `homeManager/` — Home Manager extensions and presets. Platform extensions live under `platform/programs/` and `platform/services/`.
  - `nixos/` — NixOS-only presets and profiles. No standalone platform extensions today.
  - `generic/` — Cross-platform options (`theme`, `trusted-directories`) consumed by every system substrate.
- `lib/` — Nix utilities (`buildEditor`).
- `pkgs/` — Custom package derivations.

Inside `modules/<class>/psychollama/`:

- `presets/` — single-program opinionated configs.
- `profiles/` — groupings of presets.

Module options mirror the directory structure: `psychollama.presets.programs.foo` lives at `psychollama/presets/programs/foo.nix` (or `foo/default.nix`).

Aspects follow the same rule: `dotfiles.services.syncthing` lives at `modules/aspects/dotfiles/services/syncthing.nix`.

## Conventions

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

Self-contained neovim framework in `modules/editor/`. No `~/.config` files.

- `platform/` — plugin system, LSP configuration, settings schema.
- `runtime/lua/core/` — Lua framework for Nix integration (package loading, deferred plugins, settings, LSP).
- `pkgs/dotfiles.nvim/` — neovim utilities beyond `init.vim`.

Plugin presets live under `psychollama/presets/plugins/`; LSP servers under `psychollama/presets/lsp/servers/`.

`editor` is also a Den class (`modules/den/classes/editor.nix`), so an aspect can configure Neovim beside its other classes:

```nix
dotfiles.foo = {
  homeManager.programs.git.enable = true;
  editor.extraConfig = "-- ...";
};
```

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
- Every `.nix` file under `modules/` is imported as a module. Just drop the file in. `den.nix`, `den/`, `aspects/`, and `flake/` are flake modules; the `<class>/` trees are imported through `flake.modules.*`.
- Helpers, data, and libraries opt out with an `_` prefix (`_auto-format.nix`), which import-tree ignores. `import` them explicitly where needed.
