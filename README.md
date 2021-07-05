# Dotfiles

A [NixOS](https://nixos.org/) flake that recreate my development machines.

---

| | |
|---|---|
| Shell | [zsh](https://www.zsh.org/) |
| DM | [lightdm](https://github.com/canonical/lightdm) + [enso](http://enso-os.site/) |
| WM | [XMonad](https://xmonad.org/) |
| Editor | [Neovim](http://neovim.io/) |
| Terminal | [Alacritty](https://github.com/alacritty/alacritty) |
| Launcher | [Rofi](https://github.com/davatorium/rofi) |
| Browser | [Firefox](https://www.mozilla.org/en-US/firefox/new/) |

## Usage

> :warning: Unstable :warning:  
> This project isn't polished or stable enough for other people yet. Use at your own risk.

The dotfiles framework is a NixOS module and [each machine](https://github.com/PsychoLlama/dotfiles/tree/main/hosts) is based off of it. You can use it as a flake. There's a template for convenience:

```sh
mkdir new-host && cd new-host
nix flake init --template github:PsychoLlama/dotfiles
```
