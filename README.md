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

This is managed as a nix flake. It exports the dotfiles framework and concrete definitions for each of my development machines:

```sh
# Attempts to build the machine matching your hostname
nixos-rebuild switch --flake github:PsychoLlama/dotfiles
```

Alternatively, use the exported nixos module to build your own machine:

```nix
{
  inputs.dotfiles.url = "github:PsychoLlama/dotfiles/main";

  outputs = { self, dotfiles }: {
    nixosConfiguration = nixosSystem {
      modules = [
        dotfiles.nixosModule
        ./path/to/machine.nix
      ];
    };
  };
}
```
