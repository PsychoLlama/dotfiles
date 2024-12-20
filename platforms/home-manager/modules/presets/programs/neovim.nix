{ config, lib, ... }:

with lib;

let
  cfg = config.presets.programs.neovim;
in
{
  options.presets.programs.neovim.enable = mkEnableOption "Configure Neovim as the one true editor";

  config = mkIf cfg.enable {
    home.sessionVariables =
      let
        inherit (config.programs.editor) neovim;
      in
      {
        EDITOR = "${neovim}/bin/nvim";
        MANPAGER = "${neovim}/bin/nvim -c 'Man!'";
      };

    programs.editor = {
      imports = [ ../../../../editor/modules/profiles ];
      enable = true;
      profiles.full.enable = true;
    };
  };
}
