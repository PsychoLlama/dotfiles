<div align="center">
  <h1>dotfiles</h1>
  <p>Extensible <a href="https://nixos.org/">NixOS</a> modules for configuring my personal machines.</p>
</div>

## Usage

> :construction: Frequent Breaking Changes :construction:
>
> This is my personal dotfiles framework. It changes a lot, and without notice. Harvest it for parts or inspiration, but maybe don't rely on it.

This is a set of NixOS modules and [each machine](https://github.com/PsychoLlama/dotfiles/tree/main/hosts) is based off of it. You can use it as a flake.

You can enable everything, or pick out the bits you like by enabling them piecemeal:

```nix
{
  imports = [ dotfiles.nixosModules.nixos ];

  dotfiles = {
    user.name = "gill.bates";

    # Enable everything...
    presets.kitchen-sink.enable = true;

    # Or choose your own adventure.
    presets = {
      network-management.enable = true;
      greetd.enable = true;
    };
  };
};
```

It also integrates and extends the [home-manager](https://github.com/nix-community/home-manager) framework to provide cross-platform support:

```nix
{
  home-manager.users.bob = {
    imports = [ dotfiles.nixosModules.home-manager ];

    presets = {
      desktop-environment.enable = true;
      gammastep.enable = false;
      fonts.enable = true;
    };
  };
}
```

It also integrates with macOS using the [nix-darwin](https://daiderd.com/nix-darwin/) framework:

```nix
{
  imports = [ dotfiles.nixosModules.darwin ];
}
```

Every package managed by the framework can be overridden or configured:

```nix
{
  programs.neovim.plugins = [ vimPlugins.undotree ];
  programs.sshfs.package = pkgs.sshfs.override { /* ... */ };
}
```

There's a lot more that's undocumented. Read the code to see all the options.

## Environment

- WM: [Sway](https://swaywm.org/) + [Waybar](https://github.com/Alexays/Waybar/)
- Launcher: [Rofi](https://github.com/davatorium/rofi)
- Notifications: [Dunst](https://github.com/dunst-project/dunst)
- Terminal: [Alacritty](https://github.com/alacritty/alacritty)
- Shell: [nushell](https://www.nushell.sh/) and [zsh](https://www.zsh.org/)
- Editor: [Neovim](http://neovim.io/)
- Browser: [Firefox](https://www.mozilla.org/en-US/firefox/new/)
