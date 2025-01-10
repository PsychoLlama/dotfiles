## About

All configs for my Linux, macOS, and WSL environments. I use [NixOS](https://nixos.org/) modules to manage everything as configuration-as-code. Most of it is cross platform via [home-manager](https://github.com/nix-community/home-manager). Platform bindings (NixOS, [nix-darwin](https://github.com/LnL7/nix-darwin/)) handle the rest.

This repo only manages my workstations. Servers live in [home-lab](https://github.com/PsychoLlama/home-lab/).

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
