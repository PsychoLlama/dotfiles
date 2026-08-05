# NixOS Hosts

Each host represents a different development machine, all named after fictional AIs. To build one in particular, do:

```sh
sudo nixos-rebuild switch --flake "github:PsychoLlama/dotfiles#$HOST_NAME"
```
