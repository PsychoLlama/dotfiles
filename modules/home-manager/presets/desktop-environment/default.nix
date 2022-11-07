{ config, lib, ... }:

let cfg = config.presets.desktop-environment;

in with lib; {
  imports = [ ./dunst.nix ./rofi.nix ./sway.nix ./swayidle.nix ./waybar.nix ];

  options.presets.desktop-environment.enable =
    mkEnableOption "Use an opinionated Linux desktop environment";

  config.presets = mkIf cfg.enable {
    dunst.enable = mkDefault true;
    rofi.enable = mkDefault true;
    sway.enable = mkDefault true;
    swayidle.enable = mkDefault true;
    waybar.enable = mkDefault true;
  };
}
