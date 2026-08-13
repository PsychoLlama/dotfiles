{ config, lib, ... }:

{
  options.flake.lib = lib.mkOption {
    description = "Utilities exported to downstream flakes.";
    type = lib.types.lazyAttrsOf lib.types.raw;
    default = { };
  };

  # Builds an editor with no host above it, for `nix shell dotfiles#editor`.
  config.flake.lib.editor =
    {
      pkgs,
      modules ? [ ],
      host ? { },
    }:

    let
      editor = pkgs.lib.modules.evalModules {
        class = "editor";
        modules = modules ++ [
          { _module.args.pkgs = pkgs; }
          { _module.args.host = host; }
          config.flake.editorModules.platform
        ];
      };
    in

    editor.config.neovim;
}
