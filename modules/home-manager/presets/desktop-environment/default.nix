{ config, lib, ... }:

let cfg = config.presets.desktop-environment;

in with lib; {
  imports = [ ./dunst.nix ./rofi.nix ];

  options.presets.desktop-environment.enable =
    mkEnableOption "Use an opinionated Linux desktop environment";

  config.presets = mkIf cfg.enable {
    dunst.enable = mkDefault true;
    rofi.enable = mkDefault true;
  };
}
