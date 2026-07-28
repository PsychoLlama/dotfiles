{
  lib,
  pkgs,
  ...
}:

{
  config.profiles.editor.enable = true;

  modules.home-manager =
    { config, ... }:

    let
      inherit (config.programs.editor) neovim;
    in

    {
      home.sessionVariables = {
        EDITOR = "${neovim}/bin/nvim";
        MANPAGER = "${neovim}/bin/nvim -c 'Man!'";
      };

      programs.git.ignores = [ ".vimrc.lua" ];

      programs.editor = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.neovim;
      };
    };
}
