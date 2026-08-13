# Dotfiles

NixOS-based configuration-as-code for Linux and home-manager environments.

## Architecture

This flake is consumed by other flakes. Everything must be changeable, disableable, or extendable from the outside.

Every `.nix` file under `modules/` is a flake module (the dendritic pattern). Files are organized by concern, not by module class, so one file holds every class a concern touches — `modules/aspects/programs/sway.nix` configures both NixOS and Home Manager.

Each file exports into `flake.modules.<class>.default`, typed as a `deferredModule`, so contributions from every file merge into one module per class. `modules/aspects/system/substrate.nix` assembles those into each machine. Classes are `nixos`, `homeManager`, and `editor`.

Values shared across classes are declared as flake options instead (`theme`, `identity`, `trusted-directories`, `agents`). A preset imports the declaring file and closes over `config.<option>` in an outer `let`, above the class module that shadows `config`. This is the only way the `editor` class can read them at all: it evaluates in its own module system with no platform above it.

**Importing a module is what enables it.** Nothing is gated on `enable`; evaluating the file is the side effect. Downstream removes one with `disabledModules`.

Two kinds of module share the tree:

- **Platform extensions** (`modules/platform/<class>/`) — new programs, services, and DSLs. Keep opinions out; these should be upstreamable. They only declare options, so importing one costs nothing. Unlike the rest of the tree, these _are_ filed by module class: an extension adds options to one platform's module system, so its class is the thing it extends. Everything is `homeManager` today; a `nixos`-only option would live at `platform/nixos/`.
- **Presets** — opinionated configs, imported by a profile.

Profiles (`modules/profiles/`) are the only entry points. A profile is a list of `imports`, and it publishes itself as `flake.modules.flake.<name>` so other flakes can pick it:

```nix
imports = [ dotfiles.modules.flake.linux-desktop ];
```

The repeated `flake` isn't a typo. `flake.modules.<class>` keys on module class, and a profile is a flake-parts module, so `flake` is its class the same way `nixos` is sway's. The name is load-bearing: that attribute is what stamps `_class`, and any other spelling would make the consumer's evaluation reject the module.

`modules/default.nix` auto-imports only what is safe to always evaluate: the flake's own outputs, the `rhizome` options, all of `platform/`, and the profiles. Everything else is reached by path. `platform/` is safe wholesale because extensions declare options and nothing else — evaluating one that no preset uses costs an unused option. `import-tree` skips any path containing a `/_` segment, which is what keeps helpers like `_make-program-module.nix` (a function, not a module) out of the sweep.

Auto-import serves _this_ flake only. A downstream flake importing `modules.flake.<profile>` gets exactly the profile's transitive imports, so presets must still import the platform modules they read — that duplication is deliberate, and imports are deduped.

Hosts (`modules/flake/hosts/`) hold machine-specific settings only (hardware, disk, display). All generalizable config belongs in presets. `modules/rhizome/hosts.nix` declares `options.rhizome.hosts.<hostname>`, holding the machine's `module` and `system` plus a read-only `name`, and maps them through `nixosSystem` into `flake.nixosConfigurations`. `system` is an enum over `config.systems`, so a typo'd double fails at the option rather than deep inside nixpkgs. A host is a directory of flake modules that each write into their own key, so a machine spreads across as many files as it needs (`ava/default.nix`, `ava/hardware-configuration.nix`) and they merge. `system` supplies `nixpkgs.hostPlatform`.

## Directory Structure

- `modules/` — All Nix modules, one directory per concern. `flake.nix` holds inputs only.
  - `flake/` — the flake's own outputs, one file per concern (lib, modules, nixpkgs, packages, shell, overlays, templates).
    - `hosts/` — one directory per machine, each writing into `rhizome.hosts`.
  - `rhizome/` — options this flake owns, declared for consumers (`hosts`).
  - `platform/<class>/` — the module-system layer for each class, auto-imported wholesale. `homeManager/{programs,services}/` extends upstream home-manager; `editor/` invents the `editor` class outright (see [Editor](#editor)).
  - `aspects/` — presets: everything that configures a host, none of it enabled on its own.
    - `programs/`, `services/`, `editor/` — one file (or directory) per program, service, or plugin.
    - `system/` — presets belonging to no single program (`fonts`, `gtk`, `sound-theme`), plus `substrate`.
  - `profiles/` — groupings of presets, and the only entry points. Top-level rather than under `aspects/`: a profile configures nothing itself, it selects which aspects apply.
  - `system/` — flake options shared across classes (`identity`, `theme`, `trusted-directories`, `agents`).
- `pkgs/` — Custom package derivations.

Options that survive (settings, package pins) still mirror the directory structure: `psychollama.presets.programs.foo` lives at `aspects/programs/foo.nix` (or `foo/default.nix`). A module keeps a directory when it references sibling assets relatively (`waybar/waybar.css`, `nushell/libraries/`).

## Conventions

### Platform Extensions

- Prefer upstream `home-manager`/`nixos` options. Only add custom modules when upstream lacks support.
- Prefer `home-manager` over per-OS modules; it's the most cross-platform option.
- `_make-program-module.nix` and `_mk-unstable-preset.nix` cover programs whose only options are enable and package. A program needing more imports the helper alongside its own config, keeping settings and pin in one file.

### Presets

- Single-responsibility, no `enable` option. A profile decides whether it's imported.
- Import the option modules a preset reads — its platform extension, `../../system/theme.nix`, `../../system/identity.nix`. Duplicate imports are deduped, so be generous.
- Never import another preset. Depending on one is a profile's call; guard on its upstream option instead (`lib.mkIf config.programs.direnv.enable`).
- Nested modules belong to their parent: `claude-code/default.nix` imports its own hooks, plugins and skills.
- Install programs via `programs.<name>.enable` + `programs.<name>.package`, not `home.packages`.
- Reference other programs through their `programs.<name>.package` rather than bare `pkgs.<name>`. Presets often pin `pkgs.unstable.*`, so direct references risk installing both versions.
- Resolve executable paths with `lib.getExe` (single main binary) or `lib.getExe'` (explicit binary name); bind in `let` at top of file.

## Editor

A self-contained neovim framework. No `~/.config` files. Its vocabulary is plugins and LSP servers rather than programs and services, but it splits along the same platform/preset seam as everything else, so it lives in both trees rather than a directory of its own:

- `modules/platform/editor/` — plugin system, LSP configuration, settings schema. The `editor` module class is _invented_ here rather than extended, which is what makes it a platform even though there is no upstream to defer to.
  - `runtime/lua/core/` — Lua framework for Nix integration (package loading, deferred plugins, settings, LSP). Built into a `neovim-core` plugin by `default.nix`, which pins the fileset to this directory so unrelated edits don't invalidate the derivation.
- `modules/aspects/editor/` — `plugins/` and `lsp/` presets, `profiles/` groupings. Reached from `modules/aspects/programs/editor.nix`, which is what puts neovim on a host.
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
- Every `.nix` file under `modules/` is a flake module, exporting to `flake.modules.<class>.default`.
- A new preset reaches a host only once a profile imports it. Dropping the file in does nothing.
- Always spell out `default.nix` when importing a directory module. Nix keys modules by path, so `foo` and `foo/default.nix` are distinct keys and evaluate twice — surfacing as a duplicate-definition error far from the import. Siblings (`nushell/swizzle.nix`) must be imported by that `default.nix` or listed separately.
- Helpers, data, and libraries take an `_` prefix (`_auto-format.nix`) to mark them as not-a-module. `import` them explicitly where needed.
