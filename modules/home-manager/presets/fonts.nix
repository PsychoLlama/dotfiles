{ config, lib, pkgs, ... }:

let cfg = config.presets.fonts;

in with lib; {
  options.presets.fonts.enable = mkEnableOption "Use recommended fonts";

  config = mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      fira-code
      (nerdfonts.override { fonts = [ "FiraCode" ]; })
    ];

    programs.alacritty.settings.font = rec {
      normal.family = "Fira Code";
      bold.family = normal.family;
      italic.family = normal.family;
    };
  };
}
