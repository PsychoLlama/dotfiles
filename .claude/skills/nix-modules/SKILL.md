---
name: nix-modules
description: Use when reading or writing Nix config in this repo — units, profiles, hosts and channels. Covers this repo's channels, layout and house conventions. For rhizome itself, load the `rhizome` skill.
---

- Config is composed with rhizome. The `rhizome` skill documents the library; this one covers only what is specific to this repo.
- `flake.nix` loads `units/` with `rhizome.load`. Every file takes `{ lib, rhizome, inputs, units, self, module, ... }` and returns an attrset.

## House conventions

- `unit` is what a consumer includes: a component, or a group.
- Typed input is named `cfg`.
- A unit that is a group names its own component `main`.
- A file reaches its own definitions through `self`, never `rec`.
- No NixOS or Home Manager `options`. A unit's settings are its input or a channel.

## Channels

- `nixos.module`, `home-manager.module`: a module for that class. Units write to them; the host and user scopes hand them over.
- `home-manager.lib.program "name"` installs through a Home Manager module that exists upstream. `home-manager.lib.package "name"` installs a bare package. Both from `pkgs.unstable`.
- `user.identity`, `user.trusted-directory`, `theme.palette`, `agents.memory`: facts. There is no `host` module argument.
- `nushell.abbreviation`: shell abbreviations, from any unit.
- `agents.skill`: a skill for every agent. `claude-code.settings` and `claude-code.plugin`: Claude Code only.
- A value that holds a package is a function of `pkgs` (`cmd = pkgs: [ ... ]`), or of the Home Manager module's arguments where the channel says so.
- What another unit needs that is not a channel is a function in `lib` or a constant in `values`: `swayidle.lib.disabled-flag`, `sound-theme.lib.player`, `theme.values.palettes`.

## Editor (`units/editor/`)

- `editor.plugin`, `editor.server`, `editor.config`, `editor.tool`, `editor.neovim`. Emitting a plugin installs it.
- `editor.lib.build pkgs { ... }` assembles Neovim from what was collected. `editor.unit` installs it for a user; the flake's `editor` package builds the same profile with no machine around it.

## Profiles

- A profile is a group of units.
- Profiles do not define new features: only groupings.
- Hosts prefer profiles over specific units.
- `profiles/substrate` is the baseline every host needs: package set, nix daemon, secrets.

## Hosts

- `units/hosts/<name>/default.nix` is a group around a `host.scope` holding a `user.scope`.
- The host writes the facts: identity, trusted directories, palette.
- Every host becomes `nixosConfigurations.<name>`.

## Nix conventions

- Prefer upstream `home-manager`/`nixos` options, set from a unit. Prefer `home-manager` over per-OS modules; it's the most cross-platform option.
- Prefer `config.programs.<name>.package` over `pkgs.<name>` for programs Home Manager manages, to avoid stable/unstable copies in closures.
- Resolve executables with `lib.getExe` or `lib.getExe'`. Bind at top of file.
- `git add --intent-to-add` new files so Nix can discover them.
- After moving config, compare the system derivation with `nix-diff`. A port should change ordering at most.
