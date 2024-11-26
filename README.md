## About

A set of [NixOS](https://nixos.org/) modules that manage my machines. It's like Configuration as Code for your dotfiles, but taken to an extreme. This repo manages my primary dev machine (NixOS/Linux), a few macOS devices with [nix-darwin](https://github.com/LnL7/nix-darwin/), some WSL2 environments, and a tremendous amount of cross-platform configuration between them with [home-manager](https://github.com/nix-community/home-manager).

## Linux Environment

- WM: [Sway](https://swaywm.org/) + [Waybar](https://github.com/Alexays/Waybar/)
- Launcher: [Rofi](https://github.com/davatorium/rofi)
- Notifications: [Dunst](https://github.com/dunst-project/dunst)
- Terminal: [Wezterm](https://wezfurlong.org/wezterm)
- Shell: [Nushell](https://www.nushell.sh/)
- Editor: [Neovim](http://neovim.io/)
- Browser: [Firefox](https://www.mozilla.org/en-US/firefox/new/)

## Structure

- `hosts/`: Minimal per-machine configs. They manage hardware, disk formats, or anything that can't be generalized. The rest lives in "profiles" and "presets".
- `configs/`: Opinionated modules where most of my configuration lives.
  - `presets/`: Opinionated configurations for a specific service or program.
  - `profiles/`: Groupings of presets.
- `frameworks/`: Modules extending other frameworks with new programs and settings. Many of these could be upstreamed.
  - `nix-neovim/`: My equivalent of [nixvim](https://nix-community.github.io/nixvim/).

## Other Projects

There is a separate repo for [my home lab](https://github.com/PsychoLlama/home-lab/) which manages servers and deployments on my LAN.
