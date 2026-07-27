# Dotfiles

NixOS-based configuration-as-code for Linux and home-manager environments.

## Architecture

This flake is consumed by other flakes. Everything must be changeable, disableable, or extendable from the outside.

Each platform exposes a flake-output module:

- `nixosModules.<platform>-platform` — new programs, services, and DSLs extending the platform. Keep opinions out; these should be upstreamable.

Opinionated configuration lives entirely in the top-level `modules/` plugin. Everything under `platforms/<platform>/modules/` is the platform side.

Hosts (`modules/hosts/`) hold machine-specific settings only (hardware, disk, display). All generalizable config belongs in presets.

## Directory Structure

- `modules/` — Meta-module plugin (`lib/module`). One module per program, carrying a payload for every platform it touches.
  - `hosts/` — Machine-specific configs.
  - `presets/` — single-program opinionated configs.
  - `profiles/` — groupings of presets.
- `platforms/`
  - `editor/` — Self-contained neovim framework (see [Editor](#editor)).
  - `home-manager/` — Home Manager extensions. Platform extensions live under `modules/programs/` and `modules/services/`.
- `lib/` — Nix utilities (system builders, module discovery, meta-module system, overlays).
- `pkgs/` — Custom package derivations.

`self` is the plugin's own config tree and the one handle in the system. Read a sibling by navigating it (`self.presets.programs.foo`); write by nesting under its string form (`config."${self}".presets.programs.foo.enable = true`). `global."${other}"` reaches a different plugin.

Never derive an attribute _name_ from a config read — `"${self}"` is fine, `"${self.presets.programs.foo}"` is not. Attribute names are forced while the option tree is still assembling, so a name that depends on the fixpoint is infinite recursion.

## Conventions

### Platform Extensions

- Prefer upstream `home-manager`/`nixos` options. Only add custom modules when upstream lacks support.
- Prefer `home-manager` over per-OS modules; it's the most cross-platform option.
- `makeProgramModule` exists for simple programs (enable + package only). Use standalone files when custom options are needed.

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

Plugin and language-server presets are meta-modules carrying an `editor` payload: `modules/presets/plugins/<plugin>/` and `modules/presets/lsp/servers/<server>.mod.nix`. `modules/profiles/editor/` groups them into the editor this repo ships.

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
- Only `*.mod.nix` files are discovered. Under `platforms/`, a directory entrypoint is `default.mod.nix`; under `modules/` it is `mod.nix`.
- Plain `.nix` files are free to be helpers, data, or libraries — `import` them explicitly where needed.
