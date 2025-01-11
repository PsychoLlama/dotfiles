{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.hypridle;
in

{
  options.psychollama.presets.services.hypridle = {
    enable = lib.mkEnableOption "Configure the Hypridle service";
  };

  config.services.hypridle = lib.mkIf cfg.enable {
    enable = true;
    package = lib.mkDefault pkgs.unstable.hypridle;
    settings = {
      general = {
        after_sleep_cmd = "hyprctl dispatch dpms on";
        ignore_dbus_inhibit = false;
        lock_cmd = "hyprlock";
      };

      listener = [
        {
          timeout = 900;
          on-timeout = "hyprlock";
        }
        {
          timeout = 1020;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        }
      ];
    };
  };
}
