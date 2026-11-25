# Overview

Nix configs for my workstations. Includes many custom features.

## Structure

- `pkgs/`: Custom package derivations. Overlayed as `pkgs.custom`.
  - `*.nvim/`: Custom neovim plugins.
- `flake.nix`: Inputs only; outputs derived by `flake-parts`.
- `units/`: Opinionated configs, composed with `rhizome` (typed channels). Loaded as `units.<path>`.
  - `platform/`: Levels and channels. The vocabulary every unit shares.
  - `programs/`, `services/`, `system/`, `editor/`, `agents/`: One unit per concern.
  - `profiles/`: Lists of units. Hold no opinions themselves.
  - `hosts/`: One tree per machine. Becomes `nixosConfigurations.<name>`.
- `modules/`: `flake-parts` modules.
  - `flake/`: Flake outputs.
  - `platform/<class>/`: Extensions. Matches underlying platform conventions.
    - `editor/`: Unopinionated nixvim alternative.
      - `runtime/`: Lua `core` library. Binds Nix configs.
  - `templates/`: Scaffolds other projects as `dotfiles#<tpl>`.

## Downstream

- Only me. Breaking changes are expected.
- Private workstations. Nothing else consumes this flake.

## Conventions

- Use code comments to say the bare minimum. No historical decisions. Short facts or no comment at all.

## Editing Configs

- All programs are declaratively managed.
- Default to editing Nix, not source files (e.g. `~/.claude/settings.json`).
