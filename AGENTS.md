# Dotfiles

NixOS-based configuration-as-code for Linux, macOS (nix-darwin), and home-manager environments.

## Architecture

This flake is consumed by other flakes (e.g., work machine). Everything defined here should be changeable, disableable, or extendable from the outside.

Three-tier module system:

1. **Platform modules** (`*-platform`) - Extend platforms with new programs/services/DSLs. Keep opinions out of these; they should be upstreamable.
2. **Config modules** (`*-configs`) - Opinionated configurations under the `psychollama.*` namespace.
3. **Host configs** - Machine-specific settings only (hardware, disk, display). All generalizable configs belong in presets.

Prefer `home-manager` when possible as the most cross-platform option.

Prefer `programs.foo.package` over `pkgs.foo` when referencing packages. Presets often use the unstable version, and this avoids installing both.

### Nix Style

- When defining new options, prefer namespacing like `foo.bar` instead of `fooBar`.
- Use dotted syntax for single-field attrsets (`foo.bar = "baz";`). Expand into a nested block when there are 2+ fields.

## Directory Structure

- `hosts/` - Machine-specific configs
- `platforms/` - Module definitions for each platform (editor, home-manager, nixos)
  - `modules/programs/` and `modules/services/` - Platform extensions for programs/services not available upstream
  - `modules/psychollama/presets/` - Opinionated single-program configs
  - `modules/psychollama/profiles/` - Groupings of presets
- `lib/` - Nix utilities (system builders, module discovery, etc.)

Module options mirror directory structure: `psychollama.presets.programs.foo` lives at `psychollama/presets/programs/foo.nix` (or `foo/default.nix`).

## Platform Modules

Use options from `home-manager` and `nixos` whenever possible. Only define custom modules in `modules/programs/` or `modules/services/` when the upstream platform lacks support.

## Presets and Profiles

- **Presets**: Single-responsibility modules. Only support `enable` option, no additional configuration.
- **Profiles**: Groups of presets. Consumed by hosts.
- Hosts enable profiles and may selectively enable/disable individual presets.
- Presets use `pkgs.unstable.*` for the latest package versions.
- Install packages via `programs.<name>.enable` + `programs.<name>.package = pkgs.unstable.<name>`, not `home.packages`.
- Use `lib.getExe' config.programs.<name>.package "<binary>"` for executable paths; bind these in `let` at the top of the file.

## Editor

Self-contained neovim framework in `platforms/editor/`. No `~/.config` files.

- `modules/` - Plugin system, LSP configuration, settings schema
- `runtime/lua/core/` - Core modules supporting Nix integration (manifest loading, deferred plugins, settings)
- `pkgs/lab.nvim/` - Plugin for random neovim utilities beyond `init.vim`

Plugin presets: `platforms/editor/modules/psychollama/presets/plugins/`
LSP servers: `platforms/editor/modules/psychollama/presets/lsp/servers/`

### Working with Neovim

Always check help pages when working with the neovim API:

```bash
# Find plugin help pages
nvim --headless -c 'help <name> | echo expand("%:p") | qa'

# Find the neovim runtime
nvim --headless -c 'echo $VIMRUNTIME | qa'
```

### Testing

Tests use `busted`/`vusted` framework with `*_spec.lua` naming pattern. Keep tests under `tests/`.

```bash
just unit-test  # Run Lua unit tests
```

E2E test changes by running the editor:

```bash
nix run '.#editor'
```

## Documentation

Reference docs are generated as an mdbook. Test documentation changes:

```bash
nix build '.#docs-website'
```

## Developing

All programs are declaratively managed through this repo. When asked to change configuration for a program (e.g. Claude Code settings, shell aliases, git config), edit the corresponding Nix module -- never the dotfiles directly.

- Run `just check` before committing. Keep everything passing.
- Run `just fmt` to apply code formatting.
- Run `just build` to verify the NixOS configuration compiles.
- When refactoring, use `nix eval` to verify settings are applied correctly.
- New files must be `git add`-ed before Nix can discover them.
- Nix modules in this repo are discovered and imported automatically. No need for module `imports`.
