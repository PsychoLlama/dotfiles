# Dotfiles

NixOS-based configuration-as-code for Linux and home-manager environments.

## Architecture

This flake is consumed by other flakes. Everything must be changeable, disableable, or extendable from the outside.

Two plugins ship from `modules/`, both instantiated in `flake.nix` and mounted together:

- `rhizomePlugins.dotfiles` (`modules/dotfiles/`) — the opinions. Knows nothing about hosts.
- `rhizomePlugins.hosts` (`modules/hosts/`) — machines. Takes `dotfiles` as an input and writes it through `peers."${inputs.dotfiles}"`.

The editor is the one remaining platform, exposed as `nixosModules.editor`: the framework, no opinions. Every other program carries its own home-manager payload.

Hosts hold machine-specific settings only (hardware, disk, display). All generalizable config belongs in a preset.

## Directory Structure

- `modules/` — Rhizome plugins (`lib/rhizome`). One module per program, carrying a payload for every platform it touches.
  - `dotfiles/` — single-program opinionated configs, one file (or directory) per program.
    - `programs/`, `services/` — the presets.
    - `editor/{plugins,lsp}/` — neovim plugin and language server presets.
    - `profiles/` — groupings of presets.
  - `hosts/` — Machine-specific configs.
- `editor/` — Self-contained neovim framework (see [Editor](#editor)).
- `lib/` — Nix utilities (system builders, module discovery, the rhizome module system, overlays).
- `pkgs/` — Custom package derivations.

Reads come off `self`, the plugin's own config tree — navigate it to reach a sibling (`self.programs.foo`). `cfg` is this module's own slice; `global."${inputs.<peer>}"` is another plugin's tree.

Writes go in one of three blocks, each naming a different target:

- `config` — this plugin's namespace, mount point implied (`config.programs.foo.enable = true`).
- `modules.<class>` — a host's own options, for a class this plugin declares or `lib/rhizome` builds in (`nixos`, `darwin`, `home-manager`). A block for a class the mount isn't evaluating becomes a fragment for a router to carry.
- `peers."${inputs.<peer>}"` — a peer plugin's namespace.

Nothing in attribute-name position comes from the fixpoint. Handles reach a module through `inputs`, which is load-time data supplied at instantiation, so config reads stay in value position — arbitrarily deep and lazy.

## Conventions

### Presets

- Single-responsibility. A preset owns one program, on every platform it touches.
- Prefer upstream `home-manager`/`nixos` options; write them from `modules.<class>`. Prefer `home-manager` over per-OS modules — it's the most cross-platform option.
- When upstream has no module, declare the options here (`options.package`, and whatever else the program needs) and install it yourself. There is no separate platform layer to declare them in.
- `import ./hm-program.nix "<name>"` when home-manager models the program; `import ./packaged-program.nix "<name>"` when it doesn't. Spell the module out once it grows real configuration.
- Reference other programs through `self.programs.<name>.package` (this repo's) or `config.programs.<name>.package` (home-manager's), never bare `pkgs.<name>`. Presets pin `pkgs.unstable.*`, so direct references risk installing both versions.
- Resolve executable paths with `lib.getExe` (single main binary) or `lib.getExe'` (explicit binary name); bind in `let` at top of file.
- An option a _peer_ contributes to belongs in this plugin's namespace, not home-manager's — see `programs.nushell.abbreviations`. Peers then write it without caring whether the owning program is enabled.

## Editor

Self-contained neovim framework in `editor/`. No `~/.config` files.

- `modules/` — plugin system, LSP configuration, settings schema.
- `runtime/lua/core/` — Lua framework for Nix integration (package loading, deferred plugins, settings, LSP).
- `pkgs/dotfiles.nvim/` — neovim utilities beyond `init.vim`.

Plugin and language-server presets are rhizome modules carrying an `editor` payload: `modules/dotfiles/editor/plugins/<plugin>/` and `modules/dotfiles/editor/lsp/servers/<server>.mod.nix`. `modules/dotfiles/profiles/editor/` groups them into the editor this repo ships.

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
- Only `*.mod.nix` files are discovered. Under `editor/`, a directory entrypoint is `default.mod.nix`; under `modules/` it is `mod.nix`.
- Plain `.nix` files are free to be helpers, data, or libraries — `import` them explicitly where needed.
