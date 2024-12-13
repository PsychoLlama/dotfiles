{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.presets.programs.hyprland;
in
{
  options.presets.programs.hyprland = {
    enable = lib.mkEnableOption "Opinionated Hyprland config";
  };

  config.wayland.windowManager.hyprland = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.hyprland;

    settings = {
      "$mod" = "SUPER";

      bind = [
        "$mod, Return, exec, wezterm"
        "$mod, Q, killactive,"
        "$mod SHIFT, Q, exit,"
        "$mod SHIFT, R, exec, hyprctl reload"
      ];
    };
  };
}
