{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

[
  (rhizome.component "editor" [

    (home-manager.module (
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
      }
    ))
  ])
  units.platform.editor.component
  units.editor.profiles.full.unit
]
