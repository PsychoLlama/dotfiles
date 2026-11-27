# Overview

Nix configs for my workstations. Includes many custom features.

## Structure

- `flake.nix`: Inputs and every output.
- `pkgs/`: Custom package derivations. Overlayed as `pkgs.custom`.
  - `overlays.nix`: Every overlay the package set is built with.
  - `*.nvim/`: Custom neovim plugins.
- `units/`: Everything else, composed with `rhizome` (typed channels). Loaded as `units.<path>`.
  - `platform/`: The `host` and `user` levels and scopes, and the channels every unit shares.
  - `programs/`, `services/`, `system/`, `agents/`: One unit per concern.
  - `editor/`: Neovim, assembled from channels. An unopinionated nixvim alternative.
    - `runtime/`: Lua `core` library. Reads what the build generates.
  - `profiles/`: Groups of units. Hold no opinions themselves.
  - `hosts/`: One tree per machine. Becomes `nixosConfigurations.<name>`.
- `templates/`: Scaffolds other projects as `dotfiles#<tpl>`.

## Downstream

- Only me. Breaking changes are expected.
- Private workstations. Nothing else consumes this flake.

## Conventions

- Use code comments to say the bare minimum. No historical decisions. Short facts or no comment at all.

## Editing Configs

- All programs are declaratively managed.
- Default to editing Nix, not source files (e.g. `~/.claude/settings.json`).
