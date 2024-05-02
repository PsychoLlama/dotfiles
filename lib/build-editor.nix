{
  nixpkgs,
  nixpkgs-unstable,
  tree-sitter-remix,
  self,
  ...
}:
{
  system,
  config ? { },
}:

# This utility exposes the Neovim module outside the typical module system.
# This is useful to export an editor as a flake package output.
#
#   nix shell dotfiles#editor
#
# You can still configure it declaratively in NixOS, but you can share it on
# other hosts that only have the Nix command installed.

with nixpkgs.lib;

let
  mod = modules.evalModules {
    modules = [
      self.nixosModules.editor
      { inherit config; }
    ];

    specialArgs.pkgs = import nixpkgs {
      inherit system;
      overlays = [
        tree-sitter-remix.overlays.custom-grammars
        self.overlays.latest-packages
        self.overlays.vim-plugins
      ];
    };
  };
in
mod.config.neovim
