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

    bindings =
      [
        {
          modifiers = [ "$mod" ];
          key = "Return";
          action = "wezterm";
        }
        {
          modifiers = [ "$mod" ];
          dispatcher = "killactive";
          key = "Q";
        }
        {
          modifiers = [
            "$mod"
            "SHIFT"
          ];
          dispatcher = "exit";
          key = "Q";
        }
        {
          modifiers = [ "$mod" ];
          key = "SPACE";
          action = "rofi -show drun -display-drun 'Start: '";
        }
        {
          key = "Print";
          action = pkgs.writers.writeDash "take-screenshot.sh" ''
            set -eu

            grim="${config.programs.grim.package}/bin/grim"
            slurp="${config.programs.slurp.package}/bin/slurp"
            screenshots="${config.home.homeDirectory}/screenshots"

            # Ensure this is the only screenshot instance running.
            pkill slurp || true

            region="$($slurp)" # <- Other instances terminate here.

            $grim -g "$region" "$screenshots/$(date --iso-8601=seconds).png"
          '';
        }
        {
          modifiers = [
            "$mod"
            "SHIFT"
          ];
          dispatcher = "workspace";
          key = "TAB";
          action = "m-1";
        }
        {
          modifiers = [ "$mod" ];
          dispatcher = "workspace";
          key = "TAB";
          action = "m+1";
        }
        {
          modifiers = [
            "$mod"
            "CTRL"
          ];
          key = "Q";
          action = "hyprlock --no-fade-in --immediate";
        }
      ]
      ++ lib.pipe (lib.range 1 9) [
        (map toString)
        (map (numkey: [
          # Jump to workspace
          {
            modifiers = [ "$mod" ];
            dispatcher = "workspace";
            key = toString numkey;
            action = toString numkey;
          }

          # Move current window to another workspace
          {
            modifiers = [
              "$mod"
              "SHIFT"
            ];
            dispatcher = "movetoworkspace";
            key = toString numkey;
            action = toString numkey;
          }
        ]))

        (lib.flatten)
      ];

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
