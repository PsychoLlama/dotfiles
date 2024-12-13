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

      bind =
        [
          "$mod, Return, exec, wezterm"
          "$mod, Q, killactive,"
          "$mod SHIFT, Q, exit,"
          "$mod, SPACE, exec, rofi -show drun -display-drun 'Start: '"
          ", Print, exec, grim -g \"$(slurp)\" ~/screenshots/\"$(date --iso-8601=seconds).png\""

          "$mod SHIFT, TAB, workspace, previous"
          "$mod, TAB, workspace, next"
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
    };
  };
}
