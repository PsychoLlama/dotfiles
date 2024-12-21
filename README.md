## About

A set of [NixOS](https://nixos.org/) modules that manage my machines. It's like Configuration as Code for your dotfiles, but taken to an extreme. This repo manages my primary dev machine (NixOS/Linux), a few macOS devices with [nix-darwin](https://github.com/LnL7/nix-darwin/), some WSL2 environments, and a tremendous amount of cross-platform configuration between them with [home-manager](https://github.com/nix-community/home-manager).

This repo only manages my workstations. Servers live in [home-lab](https://github.com/PsychoLlama/home-lab/).

## Linux Environment

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
