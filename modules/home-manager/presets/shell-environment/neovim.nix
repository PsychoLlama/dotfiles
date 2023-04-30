{ config, lib, ... }:

with lib;

let cfg = config.presets.neovim;

in {
  options.presets.neovim.enable =
    mkEnableOption "Configure Neovim as the one true editor";

  config = mkIf cfg.enable {
    home.sessionVariables = let inherit (config.programs.editor) neovim;

    in {
      EDITOR = "${neovim}/bin/nvim";
      MANPAGER = "${neovim}/bin/nvim -c 'Man!'";
    };

    programs.editor = {
      enable = true;
      presets.base.enable = true;
    };
  };
}
