{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.dunst;
in
{
  options.psychollama.presets.services.dunst.enable = lib.mkEnableOption "Use the dunst notification daemon";

  config.services.dunst = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dunst;

    settings = {
      global = {
        follow = "keyboard";
        font = "Monospace 12";
        frame_color = "#3f3f3f";
        frame_width = 2;
        geometry = "300x5-30+20";
        idle_threshold = 120;
        markup = "full";
        sort = true;

        max_icon_size = 32;
        min_icon_size = 0;

        mouse_left_click = "do_action, close_current";
        mouse_middle_click = "close_all";
        mouse_right_click = "close_current";
      };

      urgency_low = {
        background = "#1e1e1e";
        foreground = "#6f6f6f";
        timeout = 10;
      };

      urgency_normal = {
        background = "#1e1e1e";
        foreground = "#bfc5ce";
        timeout = 10;
      };

      urgency_critical = {
        background = "#be5046";
        foreground = "#ffffff";
        frame_color = "#e06c75";
        timeout = 0;
      };
    };
  };
}
