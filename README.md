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

- `modules/`: Opinionated config. One module per program, carrying a payload for every platform it touches.
  - `modules/hosts/`: Machine-specific configs. They manage hardware, disk formats, or anything that can't be generalized.
  - `modules/presets/`: Opinionated config for a specific program or service.
  - `modules/profiles/`: Groupings of presets.
- `platforms/`: Modules extending other platforms with new programs and services. Many of these could be upstreamed.
  - [`home-manager/`](https://github.com/nix-community/home-manager)
  - `editor/` (My equivalent of [nixvim](https://nix-community.github.io/nixvim/). Self-contained, no `~/.config` files.)

## Composition

Everything in this repo can be used piecemeal in other flakes. Modules have no side effects unless you `.enable` them.

Modules are divided into **platforms** and **configs**.

- `dotfiles.nixosModules.*-platform`: Extends platforms with new programs, services, and DSLs.
- `dotfiles.nixosModules.*-configs`: Opinionated configs bound to one platform's `psychollama.*` namespace. Mostly the editor.
- `dotfiles.plugin`: Everything else. Opinionated config for programs, services, and whole machines.

Mount the plugin on a NixOS system, then enable modules by handle.

```nix
{
  imports = [
    (dotfiles.lib.module.roots.nixos { inherit (dotfiles) plugin; })
  ];

  # Use my opinionated starship prompt.
  "${dotfiles.plugin.presets.programs.starship}".enable = true;
}
```

## Editor (neovim)

You can try my editor without installing it because it's built as a standalone package:

```nushell
nix run 'github:PsychoLlama/dotfiles#editor' ./
```

You can also build your own variant. It has access to all options from `nixosModules.editor-platform`.

```nix
flake.lib.dotfiles.buildEditor {
  inherit pkgs;
  modules = [ ];
}
```

I don't expect anyone to use these. I break stuff often. It's mostly for my own experiments.
