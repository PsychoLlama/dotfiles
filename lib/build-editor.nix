{
  nixpkgs,
  nixpkgs-unstable,
  tree-sitter-remix,
  self,
  ...
}:
{
  pkgs,
  modules ? { },
}:

# This utility exposes the Neovim module outside the typical module system.
# This is useful to export an editor as a flake package output.
#
#   nix shell dotfiles#editor
#
# You can still configure it declaratively in NixOS, but you can share it on
# other hosts that only have the Nix command installed.

let
  mod = pkgs.lib.modules.evalModules {
    specialArgs.pkgs = pkgs;
    modules = modules ++ [
      self.nixosModules.editor-platform
      self.nixosModules.editor-configs
    ];
  };
in
mod.config.neovim
