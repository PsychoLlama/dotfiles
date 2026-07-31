{ self, ... }:
{
  pkgs,
  modules ? [ ],
}:

# This utility exposes the Neovim module outside the typical module system.
# This is useful to export an editor as a flake package output.
#
#   nix shell dotfiles#editor
#
# You can still configure it declaratively in NixOS, but you can share it on
# other hosts that only have the Nix command installed.
#
# Only the framework. Presets arrive as modules — `mountEditor` is how a
# rhizome plugin becomes one.

let
  mod = pkgs.lib.modules.evalModules {
    class = "editor";
    modules = modules ++ [
      { _module.args.pkgs = pkgs; }
      self.nixosModules.editor
    ];
  };
in

mod.config.neovim
