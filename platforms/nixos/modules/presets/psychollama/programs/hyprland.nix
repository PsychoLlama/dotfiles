{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.psychollama.presets.programs.hyprland;
in

{
  options.psychollama.presets.programs.hyprland = {
    enable = lib.mkEnableOption "Use an opinionated Hyprland config";
  };

  config.programs.hyprland = lib.mkIf cfg.enable {
    enable = true;
    package = lib.mkDefault pkgs.unstable.hyprland;
    portalPackage = lib.mkDefault pkgs.unstable.xdg-desktop-portal-hyprland;
  };
}
