# Dotfiles

A [NixOS](https://nixos.org/) module that recreates my development machine.

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
