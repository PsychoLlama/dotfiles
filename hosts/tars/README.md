# Tars

WSL 2.0 on [Ava's](../ava) Windows partition. Runs on [home-manager](https://nix-community.github.io/home-manager/).

## Setup

Install Ubuntu:

```powershell
wsl --install Ubuntu-22.04
```

Next, enable systemd:

```toml
# /etc/wsl.conf
[boot]
systemd=true
```

Reboot to take effect. The rest is a standard Nix/home-manager installation.

```bash
home-manager switch --flake 'github:PsychoLlama/dotfiles'
```
