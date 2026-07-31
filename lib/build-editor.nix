{
  nixpkgs,
  nixpkgs-unstable,
  self,
  ...
}:
{
  pkgs,
  dotfiles,
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
      self.nixosModules.editor

      # No outer host to route fragments down from, so the plugin mounts
      # here. `class = "editor"` makes editor payloads merge inline; every
      # other class is discarded on purpose — a portable editor has no OS or
      # home to configure, which is the whole point of shipping it this way.
      (self.lib.rhizome.mounts.custom {
        class = "editor";
        plugins = { inherit dotfiles; };
      })
      {
        rhizome.dropped = [
          "nixos"
          "darwin"
          "homeManager"
        ];
      }
    ];
  };
in

mod.config.neovim
