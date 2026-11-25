---
name: nix-modules
description: Use when reading or writing Nix config in this repo — units, profiles, hosts, channels, and platform extensions. Covers how the tree is loaded, what each kind of file returns, and the conventions they follow.
---

## Units (`units/`)

- Composed with the `rhizome` flake input. Its README describes the model: level, channel, component, scope.
- `units/default.nix` loads the directory as one attrset. Path is attribute path: `programs/git.nix` is `units.programs.git`.
- Every file takes `{ lib, rhizome, inputs, units, ... }`.
- A unit is a component, or a list when it brings other units with it.
- A directory's `default.nix` is the directory's `unit`: `programs/tmux/default.nix` is `units.programs.tmux.unit`. When it returns named things instead, they sit beside the directory's files.
- Files prefixed `_` are not loaded. Use it for plain modules and helpers.
- Including a unit is the side effect. No `enable` gating.
- A unit named by two lists is included once.

## Channels (`units/platform/`)

- `nixos.module`, `home-manager.module`, `editor.module`: a module for that class. Units write to them; the host and user scopes hand them over.
- `home-manager.program "name"`: installs a program from `pkgs.unstable` through its Home Manager module.
- `user.identity`, `user.trusted-directory`, `theme.palette`, `agents.*`: facts. A unit that needs one names it in `uses` and receives the value in `config`.
- Facts are not module arguments. There is no `host` argument.

## Profiles

- A profile is a list of units.
- Profiles do not define new features: only groupings.
- Hosts prefer profiles over specific units.
- `profiles/substrate` is the baseline every host needs: package set, nix daemon, home-manager, secrets.

## Hosts

- `units/hosts/<name>/default.nix` is a `nixos.host` scope holding a `home-manager.user` scope.
- The host writes the facts: identity, trusted directories, palette.
- Every host becomes `nixosConfigurations.<name>`.

## Extensions (`modules/platform/`)

- Platform extensions overlay existing options with new capabilities.
- Prefer upstream `home-manager`/`nixos` options. Only add custom modules when upstream lacks support.
- Prefer `home-manager` over per-OS modules; it's the most cross-platform option.
- Simple programs (`enable` + `package`) are generated inline by `platform/homeManager/programs/default.nix`.

## Conventions

- Install programs via `programs.<name>.enable` + `programs.<name>.package`, not `home.packages`.
- Prefer `config.programs.<name>.package` over `pkgs.<name>` to avoid stable/unstable copies in closures.
- Resolve executables with `lib.getExe` or `lib.getExe'`. Bind at top of file.
- `import-tree` auto-imports Nix files under `modules/` as `flake-parts` modules. Prefix with `_` if you need control over import.
- `git add --intent-to-add` new files so Nix can discover them.
- After moving config, compare the system derivation with `nix-diff`. A port should change ordering at most.
