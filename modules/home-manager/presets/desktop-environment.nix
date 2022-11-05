{ config, lib, ... }:

let cfg = config.presets.desktop-environment;

in with lib; {
  options.presets.desktop-environment.enable =
    mkEnableOption "Use an opinionated Linux desktop environment";

  config = mkIf cfg.enable {
    presets.dunst.enable = mkDefault true;
    presets.rofi.enable = mkDefault true;
  };
}
