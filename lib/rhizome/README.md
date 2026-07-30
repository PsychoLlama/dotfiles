# Rhizome

A tiny framework for keeping NixOS modules maintainable.

## Features

- **Isolation:** Each Rhizome plugin gets its own module namespace.
- **Multi-platform:** One module can configure nixos, home-manager, nix-darwin, etc.
- **Composable:** Plugins can build on other plugins.
- **Identifiable:** Options mirror the file system so you always know where to find the definition.

## Purpose

I had trouble scaling my dotfiles. Configs that affected more than one platform bled across multiple files. Options needed prefixes to avoid conflicts with the platform. Side effects leaked if I forgot an `enable` guard.

Rhizome grew out of a goal to fix these pain points.

## Usage

Purely internal for this repo. I may publish it separately in the future.
