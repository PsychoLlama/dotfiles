{
  imports = [ ../editor/profiles/full/default.nix ];

  exports.homeManager =
    {
      config,
      lib,
      pkgs,
      ...
    }:

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
