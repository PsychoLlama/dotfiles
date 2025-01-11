## About

All configs for my Linux, macOS, and WSL environments. I use [NixOS](https://nixos.org/) modules to manage everything as configuration-as-code. Most of it is cross platform via [home-manager](https://github.com/nix-community/home-manager). Platform bindings (NixOS, [nix-darwin](https://github.com/LnL7/nix-darwin/)) handle the rest.

This repo only manages my workstations. Servers live in [home-lab](https://github.com/PsychoLlama/home-lab/).

![Linux desktop screenshot](https://github.com/user-attachments/assets/f914a67e-85cf-41c9-95b3-3c418a521c4f)

<p align="center"><em>Ava, my linux workstation</em></p>

## Stack

- WM: [Hyprland](https://hyprland.org/) + [Waybar](https://github.com/Alexays/Waybar/)
- Launcher: [Rofi](https://github.com/davatorium/rofi)
- Notifications: [Dunst](https://github.com/dunst-project/dunst)
- Terminal: [Wezterm](https://wezfurlong.org/wezterm)
- Shell: [Nushell](https://www.nushell.sh/)
- Editor: [Neovim](http://neovim.io/)
- Browser: [Firefox](https://www.mozilla.org/en-US/firefox/new/)

## Structure

- `hosts/`: Machine-specific configs. They manage hardware, disk formats, or anything that can't be generalized.
- `platforms/`: Modules extending other platforms with new programs and services. Many of these could be upstreamed.
  - [`home-manager/`](https://github.com/nix-community/home-manager)
  - [`nixos/`](https://nixos.org/)
  - `editor/` (My equivalent of [nixvim](https://nix-community.github.io/nixvim/). Self-contained, no `~/.config` files.)
- `platforms/*/modules/presets/`: Opinionated config for a specific program or service.
- `platforms/*/modules/profiles/`: Groupings of presets.

## Composition

Everything in this repo can be used piecemeal in other flakes. Modules have no side effects unless you `.enable` them.

Modules are divided into **platforms** and **configs**.

- `dotfiles.nixosModules.*-platform`: Extends platforms with new programs, services, and DSLs.
- `dotfiles.nixosModules.*-config`: Opinionated configurations for programs and services.

Configs are available under the `psychollama.*` namespace.

```nix
# Use my opinionated starship prompt.
config.psychollama.presets.starship.enable = true;
```

## Editor (neovim)

You can try my editor without installing it because it's built as a standalone package:

```nushell
nix run 'github:PsychoLlama/dotfiles#editor' ./
```

You can also build your own variant. It have access to all options from `nixosModules.editor-platform`.

```nix
flake.lib.dotfiles.buildEditor {
  inherit pkgs;
  modules = [ ];
}
```

I don't expect anyone to use these. I break stuff often. It's mostly for my own experiments.

## Documentation

Reference documentation [generated and published here](https://psychollama.github.io/dotfiles/).
