## About

All configs for my Linux environment. I use [NixOS](https://nixos.org/) modules to manage everything as configuration-as-code. Most of it is cross platform via [home-manager](https://github.com/nix-community/home-manager). NixOS platform bindings handle the rest.

This repo only manages my workstations. Servers live in [home-lab](https://github.com/PsychoLlama/home-lab/).

![Linux desktop screenshot](https://github.com/user-attachments/assets/f914a67e-85cf-41c9-95b3-3c418a521c4f)

<p align="center"><em>Ava, my linux workstation</em></p>

## Stack

- WM: [Sway](https://swaywm.org/) + [Waybar](https://github.com/Alexays/Waybar/)
- Launcher: [Fuzzel](https://codeberg.org/dnkl/fuzzel)
- Notifications: [Dunst](https://github.com/dunst-project/dunst)
- Terminal: [Wezterm](https://wezfurlong.org/wezterm)
- Shell: [Nushell](https://www.nushell.sh/)
- Editor: [Neovim](http://neovim.io/)
- Browser: [Chromium](https://www.chromium.org/Home/)

## Structure

- `modules/`: Two [Rhizome](./lib/rhizome/README.md) plugins:
  - `modules/dotfiles/`: Opinionated modules per program/service, plus `profiles/` grouping them.
  - `modules/hosts/`: Machine-specific configs. They manage hardware, disk formats, or anything that can't be generalized.
- `editor/`: The neovim framework. My equivalent of [nixvim](https://nix-community.github.io/nixvim/). Self-contained, no `~/.config` files.

## Editor (neovim)

You can try my editor without installing it because it's built as a standalone package:

```nushell
nix run 'github:PsychoLlama/dotfiles#editor' ./
```

You can also build your own variant. It has access to all options from `nixosModules.editor`.

```nix
inputs.dotfiles.lib.buildEditor {
  inherit pkgs;
  modules = [ { extraConfig = "set number"; } ];
}
```

I don't expect anyone to use it. I break stuff often. It's mostly for my own experiments.
