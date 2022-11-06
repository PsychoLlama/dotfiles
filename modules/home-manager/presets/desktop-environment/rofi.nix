{ config, lib, ... }:

let cfg = config.presets.rofi;

in with lib; {
  options.presets.rofi.enable = mkEnableOption "Use the rofi launcher";

  config.programs.rofi = mkIf cfg.enable {
    enable = true;
    theme = ../../../../config/rofi/theme.rasi;
    extraConfig.modi = "drun,run";
  };
}
