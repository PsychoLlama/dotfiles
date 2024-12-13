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

      # The default scaled my display to 1.5. Not sure why. Overriding.
      monitor = ",preferred,auto,1";

      # Fast and crisp.
      animations.enabled = false;
      decoration.blur.enabled = false;

      exec-once =
        [ ]
        ++ (lib.optional config.programs.waybar.enable "waybar")
        ++ (lib.optional config.services.swaybg.enable "systemctl start --user swaybg");

      bind =
        [
          "$mod, Return, exec, wezterm"
          "$mod, Q, killactive,"
          "$mod SHIFT, Q, exit,"
          "$mod, SPACE, exec, rofi -show drun -display-drun 'Start: '"
          ", Print, exec, grim -g \"$(slurp)\" ~/screenshots/\"$(date --iso-8601=seconds).png\""

          "$mod SHIFT, TAB, workspace, m-1"
          "$mod, TAB, workspace, m+1"

          "$mod CTRL, Q, exec, hyprlock"
        ]
        ++ lib.pipe (lib.range 1 9) [
          (map toString)
          (map (numkey: [
            # Jump to workspace
            "$mod, ${numkey}, workspace, ${numkey}"

            # Move current window to another workspace
            "$mod SHIFT, ${numkey}, movetoworkspace, ${numkey}"
          ]))

          (lib.flatten)
        ];

      input = {
        kb_layout = "us";
        kb_options = "caps:escape";
        repeat_delay = "200";
        touchpad.natural_scroll = "yes";
      };

      general = {
        gaps_in = 0;
        gaps_out = 0;
        border_size = 0;
        no_border_on_floating = true;
      };

      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
      };
    };
  };
}
