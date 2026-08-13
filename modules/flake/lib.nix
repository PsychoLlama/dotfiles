{
  config,
  inputs,
  lib,
  ...
}:

{
  options.flake = inputs.flake-parts.lib.mkSubmoduleOptions {
    lib = lib.mkOption {
      description = "Utilities exported to downstream flakes.";
      type = lib.types.lazyAttrsOf lib.types.raw;
      default = { };
    };
  };

  # This utility exposes the Neovim module outside the typical module system.
  # This is useful to export an editor as a flake package output.
  #
  #   nix shell dotfiles#editor
  #
  # You can still configure it declaratively in NixOS, but you can share it on
  # other hosts that only have the Nix command installed.
  config.flake.lib.editor =
    {
      pkgs,
      modules ? [ ],
    }:

    let
      editor = pkgs.lib.modules.evalModules {
        class = "editor";
        modules = modules ++ [
          { _module.args.pkgs = pkgs; }
          config.flake.modules.editor.platform
        ];
      };
    in

    editor.config.neovim;
}
