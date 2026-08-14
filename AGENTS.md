# Overview

Nix configs for my workstations. Includes many custom features.

## Structure

- `pkgs/`: Custom package derivations. Overlayed as `pkgs.custom`.
  - `*.nvim/`: Custom neovim plugins.
- `flake.nix`: Inputs only; outputs derived by `flake-parts`.
- `modules/`: All Nix modules.
  - `flake/`: Flake outputs.
  - `rhizome/`: Custom flake tools for managing hosts and aspects.
    - `substrate/`: Opinionated defaults for all hosts.
  - `platform/<class>/`: Extensions. Matches underlying platform conventions.
    - `editor/`: Unopinionated nixvim alternative.
      - `runtime/`: Lua `core` library. Binds Nix configs.
  - `aspects/`: Opinionated configs enabled by hosts. One file per concern.
    - `system/`: Aspects belonging to no single program.
    - `profiles/`: Holds no opinions itself; enables sets of other aspects.
    - `editor/`: Opinionated editor aspects.
  - `hosts/`: Workstation configs. One dir per host. Only contains workstation-specific configs - the rest is aspects and profiles.
  - `templates/`: Scaffolds other projects as `dotfiles#<tpl>`.

## Downstream

- Only me. Breaking changes are expected.
- Private workstations. Work computers consume and build on this flake.

## Conventions

- Use code comments to say the bare minimum. No historical decisions. Short facts or no comment at all.

## Editing Configs

- All programs are declaratively managed.
- Default to editing Nix, not source files (e.g. `~/.claude/settings.json`).
