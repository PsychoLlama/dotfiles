# Dotfiles

A [NixOS](https://nixos.org/) module that recreates my development machine.

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

Uh, don't. This isn't generic or stable enough to be useful for other people yet. I'd recommend forking instead.

This is managed as a custom nix channel:

```sh
nix-channel --add https://github.com/PsychoLlama/dotfiles/archive/main.tar.gz dotfiles
nix-channel --update dotfiles
```

Then include it in your `configuration.nix` as an import and set the options for your user account:

```nix
{
  imports = [
    # ... other modules come first
    <dotfiles>
  ];

  dotfiles.user = {
    account = "lasershark";
    fullName = "Shark Laserman";
  };
}
```
