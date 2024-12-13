{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.presets.programs.hyprlock;
in

{
  options.presets.programs.hyprlock = {
    enable = lib.mkEnableOption "Opinionated hyprlock config";
  };

  config.programs.hyprlock = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.hyprlock;

    settings = {
      general = {
        disable_loading_bar = true;
        grace = 0;
        hide_cursor = true;
        no_fade_in = false;
      };

      background = [
        {
          path = "attic/images/wallpapers/current";
          blur_passes = 1;
          blur_size = 8;
        }
      ];

      input-field = [
        {
          size = "200, 50";
          position = "0, -80";
          monitor = "";
          dots_center = true;
          fade_on_empty = false;
          font_color = "rgb(202, 211, 245)";
          inner_color = "rgb(91, 96, 120)";
          outer_color = "rgb(24, 25, 38)";
          outline_thickness = 5;
          shadow_passes = 2;
        }
      ];
    };
  };
}
