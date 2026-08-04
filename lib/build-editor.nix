{
  nixpkgs,
  nixpkgs-unstable,
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
    class = "editor";
    modules = modules ++ [
      { _module.args.pkgs = pkgs; }
      self.modules.editor.platform
      self.modules.generic.configs
    ];
  };
in

mod.config.neovim
