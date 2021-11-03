# Dotfiles

A [NixOS](https://nixos.org/) flake that recreate my development machines.

---

| Category | Tool |
|---|--:|
| WM | [Sway](https://swaywm.org/) + [Waybar](https://github.com/Alexays/Waybar/) |
| Launcher | [Rofi](https://github.com/davatorium/rofi) |
| Notifications | [Dunst](https://github.com/dunst-project/dunst) |
| Terminal | [Alacritty](https://github.com/alacritty/alacritty) |
| Shell | [zsh](https://www.zsh.org/) |
| Editor | [Neovim](http://neovim.io/) |
| Browser | [Firefox](https://www.mozilla.org/en-US/firefox/new/) |

## Usage

> :warning: Unstable :warning:  
> This project isn't polished or stable enough for other people yet. Use at your own risk.

The dotfiles framework is a NixOS module and [each machine](https://github.com/PsychoLlama/dotfiles/tree/main/hosts) is based off of it. You can use it as a flake. There's a template for convenience:

```sh
mkdir new-host && cd new-host
nix flake init --template github:PsychoLlama/dotfiles
```

You can enable everything, or pick out the pieces you like by enabling them piecemeal:

```nix
{
  # Enable everything...
  dotfiles.kitchen-sink.enable = true;

  # OR choose your own adventure.
  dotfiles.editor = {
    enable = true;
    config = ./path/to/config.vim;
    linter.enable = true;
  };

  dotfiles.chat-client = {
    enable = true;
    matrix.enable = true;
    slack.enable = true;
  };

  dotfiles.toolkit.rust-development.enable = true;

  # ... and many other options. Check out `modules/` for more.
}
```

## Inspiration

Thanks to [hlissner's dotfiles](https://github.com/hlissner/dotfiles/) for being so damn good. I learned a lot from his code.
