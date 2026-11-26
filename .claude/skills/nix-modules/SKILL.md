---
name: nix-modules
description: Use when reading or writing Nix config in this repo — units, profiles, hosts and channels. Covers how the tree is loaded, what each kind of file returns, and the conventions they follow.
---

## Units (`units/`)

- Composed with the `rhizome` flake input. Its README describes the model: level, channel, component, scope, unit.
- `rhizome.load` mirrors the directory as one attrset. Path is attribute path: `programs/git.nix` is `units.programs.git`. A directory's `default.nix` attributes sit beside its files.
- Every file takes `{ lib, rhizome, inputs, units, ... }` and returns `rhizome.unit "<name>" { ... }`, which names each definition after its attribute.
- What a consumer includes is always `unit`: `units.programs.git.unit`. A unit that brings others with it is a list, with its own component named `main`.
- Refer to siblings through the unit's argument, named after the unit: `rhizome.unit "nushell" (nushell: { ... nushell.abbreviation ... })`. Never `rec`.
- Files prefixed `_` are not loaded. Use it for plain modules and helpers.
- Including a unit is the side effect. No `enable` gating, and no NixOS or Home Manager `options`.
- A unit named by two lists is included once.

## Channels

- A channel belongs to the unit that reads it: `nushell.abbreviation`, `claude-code.settings`, `agents.skill`, `editor.plugin`.
- `nixos.module`, `home-manager.module`: a module for that class. Units write to them; the host and user scopes hand them over.
- `home-manager.program "name"` installs through a Home Manager module that exists upstream. `home-manager.package "name"` installs a bare package. Both from `pkgs.unstable`.
- `user.identity`, `user.trusted-directory`, `theme.palette`, `agents.memory`: facts. A unit that needs one names it in `uses` and receives the value in `config`.
- Facts are not module arguments. There is no `host` argument.
- The graph is evaluated before any package set exists. A value that holds a package is a function of `pkgs` (`cmd = pkgs: [ ... ]`), or of the Home Manager module's arguments where the channel says so.
- What another unit needs from one that is not a channel is a plain attribute of it: `swayidle.disabled-flag`, `sound-theme.player`.

## Editor (`units/editor/`)

- `editor.plugin`, `editor.server`, `editor.config`, `editor.tool`, `editor.neovim`. Emitting a plugin installs it.
- `editor.lib.build pkgs { ... }` assembles Neovim from what was collected. `editor.unit` installs it for a user; the flake's `editor` package builds the same profile with no machine around it.

## Profiles

- A profile is a list of units.
- Profiles do not define new features: only groupings.
- Hosts prefer profiles over specific units.
- `profiles/substrate` is the baseline every host needs: package set, nix daemon, secrets.

## Hosts

- `units/hosts/<name>/default.nix` is a `host.scope` holding a `user.scope`.
- The host writes the facts: identity, trusted directories, palette.
- Every host becomes `nixosConfigurations.<name>`.

## Conventions

- Prefer upstream `home-manager`/`nixos` options, set from a unit. Prefer `home-manager` over per-OS modules; it's the most cross-platform option.
- Prefer `config.programs.<name>.package` over `pkgs.<name>` for programs Home Manager manages, to avoid stable/unstable copies in closures.
- Resolve executables with `lib.getExe` or `lib.getExe'`. Bind at top of file.
- `git add --intent-to-add` new files so Nix can discover them.
- After moving config, compare the system derivation with `nix-diff`. A port should change ordering at most.
