{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.fonts;

in {
  options.presets.fonts.enable = mkEnableOption "Use recommended fonts";

  config = mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    home.packages = with pkgs.unstable; [
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
