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
      self.nixosModules.editor-platform
      self.nixosModules.universal-platform

      # No outer root to route fragments down from, so the plugin mounts
      # here. `class = "editor"` makes editor payloads merge inline; the
      # nixos and home-manager ones pile up in `_meta.fragments` unread.
      (self.lib.module.mkRoot {
        class = "editor";
        plugins.dotfiles = self.plugin;
      })
    ];
  };
in

mod.config.neovim
