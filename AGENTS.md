# Dotfiles

NixOS-based configuration-as-code for Linux and home-manager environments.

## Architecture

This flake is consumed by other flakes. Everything must be changeable, disableable, or extendable from the outside.

Every `.nix` file under `modules/` is a flake module (the dendritic pattern). Files are organized by concern, not by module class, so one file holds every class a concern touches — `modules/aspects/programs/sway.nix` configures both NixOS and Home Manager.

Classes are `nixos`, `homeManager`, and `editor`. Where a file publishes depends on which of the two trees it is in.

Values shared across classes are declared as flake options instead (`theme`, `identity`, `trusted-directories`, `agents`), under `modules/rhizome/`. An aspect imports the declaring file and closes over `config.<option>` in an outer `let`, above the class module that shadows `config`. This is the only way the `editor` class can read them at all: it evaluates in its own module system with no platform above it.

Two kinds of module share the tree:

- **Platform extensions** (`modules/platform/<class>/`) — new programs, services, and DSLs. Keep opinions out; these should be upstreamable. They only declare options, so importing one costs nothing. Unlike the rest of the tree, these _are_ filed by module class: an extension adds options to one platform's module system, so its class is the thing it extends. Everything is `homeManager` today; a `nixos`-only option would live at `platform/nixos/`. Extensions publish into `flake.modules.<class>.default`, which every host loads unconditionally.
- **Aspects** (`modules/aspects/`) — opinionated configs, selected by a profile. Each publishes under its own id.

`modules/default.nix` sweeps `flake/`, `rhizome/`, and `platform/` with `import-tree`, and sweeps `aspects/` through `import-tree.map (import-aspect ./aspects)`. `import-tree` skips any path containing a `/_` segment, which is what keeps helpers like `_make-program-module.nix` (a function, not a module) out of the sweep.

## Aspects

An aspect may set exactly two attributes, asserted by the loader:

```nix
{
  imports = [ ../../platform/homeManager/programs/bat.nix ];

  exports.homeManager =
    { lib, ... }:
    { programs.bat.enable = lib.mkDefault true; };
}
```

`exports.<class>` is the module for that class. The class list is closed, so a typo is an error rather than an attribute nobody reads. Anything else — a bare `flake.modules`, a stray `perSystem` — is rejected.

The loader turns the file into `flake.modules.<class>.<id>`, where `<id>` is the path relative to `aspects/` without the extension: `aspects/programs/nushell/default.nix` becomes `programs/nushell`. Every id is published to every class, empty where the aspect exports nothing, so a pure aggregator is still walkable.

`imports` is split three ways by what the entry is:

| entry                                           | treatment                                     |
| :---------------------------------------------- | :-------------------------------------------- |
| a path under `aspects/`                         | recorded as a dependency id, **not** imported |
| a value with no path (`import ./_helper.nix x`) | checked, and its exports fold into the parent |
| any other path                                  | an ordinary flake module, passed through      |

The first case is why an aspect never imports another aspect's file into itself: the sweep already published it, and importing it again is a second module key for the same file — which surfaces as a duplicate-definition error far from the cause.

A dependency becomes an `imports` entry on the published module, pointing at the dependency's module for the same class. So `flake.modules.nixos."profiles/full"` already carries its whole transitive tree; nothing has to walk it.

Two details make that work:

- The exported module reaches its dependencies through the flake's `config`, which the same aspect's own `imports` could not do without infinite recursion. A class module evaluates under NixOS or Home Manager — a different module system — where the flake's `config` is an ordinary closed-over value.
- Each published module sets `key = "aspect:<class>:<id>"`. Two aspects depending on a third contribute the same key, so the module system loads it once. Without it, each route is a distinct module and the file's options are declared twice. `flake-parts` does not set a key itself, but one set on our own attrset survives the wrapping.

A dependency cycle recurses for real (`stack overflow`, when the module is evaluated). Nothing in the tree has one.

## Profiles

Profiles (`modules/aspects/profiles/`) are aspects that only have `imports`. They select; they configure nothing themselves. A host names them by id:

```nix
rhizome.hosts.ava.profiles = [ "profiles/full" "profiles/linux-desktop" ];
```

`rhizome/hosts.nix` looks those up in `flake.modules.nixos`; `rhizome/substrate.nix` looks up the same ids in `flake.modules.homeManager` (for `sharedModules`) and `flake.modules.editor` (for the `programs.editor` submodule). There is no resolver in between — a published module already imports its dependencies, so the lookup is the whole job.

`profiles` is an enum over the published ids, for the same reason `system` is one: a typo names the host and the misspelled id instead of surfacing as a missing attribute wherever the module is finally used.

A consumer picks the same way, since the sweep publishes every aspect regardless of what any profile selects:

```nix
imports = [ dotfiles.modules.nixos."profiles/linux-desktop" ];
```

Aspects must still import the platform modules and `rhizome/` options they read — that duplication is deliberate, and imports are deduped.

## Hosts

Hosts (`modules/flake/hosts/`) hold machine-specific settings only (hardware, disk, display). All generalizable config belongs in aspects. `modules/rhizome/hosts.nix` declares `options.rhizome.hosts.<hostname>`, holding the machine's `module`, `profiles`, and `system` plus a read-only `name`, and maps them through `nixosSystem` into `flake.nixosConfigurations`. `system` is an enum over `config.systems`, so a typo'd double fails at the option rather than deep inside nixpkgs. A host is a directory of flake modules that each write into their own key, so a machine spreads across as many files as it needs (`ava/default.nix`, `ava/hardware-configuration.nix`) and they merge. `system` supplies `nixpkgs.hostPlatform`.

## Directory Structure

- `modules/` — All Nix modules, one directory per concern. `flake.nix` holds inputs only.
  - `flake/` — the flake's own outputs, one file per concern (lib, modules, nixpkgs, packages, shell, overlays, templates).
    - `hosts/` — one directory per machine, each writing into `rhizome.hosts`.
  - `rhizome/` — the machinery this flake owns, always evaluated: the `hosts` option, the aspect loader (`_aspects.nix`), the flake options shared across classes (`identity`, `theme`, `trusted-directories`, `agents`), and `substrate.nix` — the nixpkgs/Nix-daemon/Home-Manager base every machine is built on. The substrate lives here rather than under `aspects/` because it reads flake inputs, which a consumer's flake does not have.
  - `platform/<class>/` — the module-system layer for each class, auto-imported wholesale. `homeManager/{programs,services}/` extends upstream home-manager; `editor/` invents the `editor` class outright (see [Editor](#editor)).
  - `aspects/` — everything that configures a host, none of it applied on its own.
    - `programs/`, `services/`, `editor/` — one file (or directory) per program, service, or plugin.
    - `system/` — aspects belonging to no single program (`fonts`, `gtk`, `sound-theme`).
    - `profiles/` — groupings, and the only thing a host names.
- `pkgs/` — Custom package derivations.

Options that survive (settings, package pins) still mirror the directory structure: `psychollama.presets.programs.foo` lives at `aspects/programs/foo.nix` (or `foo/default.nix`). A module keeps a directory when it references sibling assets relatively (`waybar/waybar.css`, `nushell/libraries/`).

## Conventions

### Platform Extensions

- Prefer upstream `home-manager`/`nixos` options. Only add custom modules when upstream lacks support.
- Prefer `home-manager` over per-OS modules; it's the most cross-platform option.
- `_make-program-module.nix` and `_mk-unstable-preset.nix` cover programs whose only options are enable and package. A program needing more imports the helper alongside its own config, keeping settings and pin in one file.

### Aspects

- Single-responsibility, no `enable` option. A profile decides whether it applies.
- Import the option modules an aspect reads — its platform extension, `../../rhizome/theme.nix`, `../../rhizome/identity.nix`. Duplicate imports are deduped, so be generous.
- Only a profile should import another aspect for the sake of turning it on. Depending on one from an aspect is fine when the aspect genuinely cannot work without it (`claude-code/default.nix` imports its own hooks, plugins and skills); depending on one you merely want present is a profile's call, so guard on its upstream option instead (`lib.mkIf config.programs.direnv.enable`).
- Install programs via `programs.<name>.enable` + `programs.<name>.package`, not `home.packages`.
- Reference other programs through their `programs.<name>.package` rather than bare `pkgs.<name>`. Presets often pin `pkgs.unstable.*`, so direct references risk installing both versions.
- Resolve executable paths with `lib.getExe` (single main binary) or `lib.getExe'` (explicit binary name); bind in `let` at top of file.

## Editor

A self-contained neovim framework. No `~/.config` files. Its vocabulary is plugins and LSP servers rather than programs and services, but it splits along the same platform/aspect seam as everything else, so it lives in both trees rather than a directory of its own:

- `modules/platform/editor/` — plugin system, LSP configuration, settings schema. The `editor` module class is _invented_ here rather than extended, which is what makes it a platform even though there is no upstream to defer to.
  - `runtime/lua/core/` — Lua framework for Nix integration (package loading, deferred plugins, settings, LSP). Built into a `neovim-core` plugin by `default.nix`, which pins the fileset to this directory so unrelated edits don't invalidate the derivation.
- `modules/aspects/editor/` — `plugins/` and `lsp/` aspects, `profiles/` groupings. Reached from `modules/aspects/programs/editor.nix`, which is what puts neovim on a host.
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
- Every `.nix` file under `modules/` is a flake module. Under `aspects/` it sets `exports.<class>`; everywhere else it sets `flake.modules.<class>.default` directly.
- A new aspect reaches a host only once a profile imports it. Dropping the file in publishes it and nothing more.
- Always spell out `default.nix` when importing a directory module. Nix keys modules by path, so `foo` and `foo/default.nix` are distinct keys and evaluate twice — surfacing as a duplicate-definition error far from the import. Siblings (`nushell/swizzle.nix`) must be imported by that `default.nix` or listed separately.
- Helpers, data, and libraries take an `_` prefix (`_auto-format.nix`) to mark them as not-a-module. `import` them explicitly where needed.
