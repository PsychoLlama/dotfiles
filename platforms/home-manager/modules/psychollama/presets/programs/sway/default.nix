{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.sway;
  theme = config.theme.palette;
  mod = "Mod4";
in

{
  options.psychollama.presets.programs.sway.enable =
    lib.mkEnableOption "Use SwayWM as the desktop environment";

  config = lib.mkIf cfg.enable {
    wayland.windowManager.sway = {
      enable = true;
      package = null;
      checkConfig = false;
      systemd.enable = true;

      config = {
        modifier = mod;
        terminal = "wezterm";
        menu = "fuzzel";
        modes = { };

        keybindings = {
          # Lock the computer.
          "${mod}+Control+q" = "exec swaylock";
          "${mod}+q" = "kill";

          "${mod}+Return" = "exec wezterm";
          "${mod}+space" = "exec fuzzel";

          # Emoji picker.
          "${mod}+period" = "exec bemoji -t";

          # Screenshot: select region, save to ~/screenshots/, copy to clipboard.
          "Print" =
            ''exec grim -g "$(slurp)" - | tee ~/screenshots/"$(date --iso-8601=seconds).png" | wl-copy'';

          "${mod}+Shift+r" = "reload";

          # Focus.
          "${mod}+h" = "focus left";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";

          # Move windows.
          "${mod}+Shift+h" = "move left";
          "${mod}+Shift+j" = "move down";
          "${mod}+Shift+k" = "move up";
          "${mod}+Shift+l" = "move right";

          # Workspaces.
          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 10";

          "${mod}+Tab" = "workspace next";
          "${mod}+Shift+Tab" = "workspace prev";

          # Move focused container to workspace.
          "${mod}+Shift+1" = "move container to workspace number 1; workspace number 1";
          "${mod}+Shift+2" = "move container to workspace number 2; workspace number 2";
          "${mod}+Shift+3" = "move container to workspace number 3; workspace number 3";
          "${mod}+Shift+4" = "move container to workspace number 4; workspace number 4";
          "${mod}+Shift+5" = "move container to workspace number 5; workspace number 5";
          "${mod}+Shift+6" = "move container to workspace number 6; workspace number 6";
          "${mod}+Shift+7" = "move container to workspace number 7; workspace number 7";
          "${mod}+Shift+8" = "move container to workspace number 8; workspace number 8";
          "${mod}+Shift+9" = "move container to workspace number 9; workspace number 9";
          "${mod}+Shift+0" = "move container to workspace number 10; workspace number 10";

          # Layout.
          "${mod}+b" = "splith";
          "${mod}+v" = "splitv";
          "${mod}+s" = "layout stacking";
          "${mod}+w" = "layout tabbed";
          "${mod}+e" = "layout toggle split";
          "${mod}+f" = "fullscreen";

          # Playback control (laptop has no media keys).
          "Insert" = "exec playerctl play-pause";
          "Home" = "exec playerctl previous";
          "End" = "exec playerctl next";

          "XF86AudioPlay" = "exec playerctl play-pause";
          "XF86AudioPrev" = "exec playerctl previous";
          "XF86AudioNext" = "exec playerctl next";

          "XF86MonBrightnessDown" = "exec brightnessctl set 10%-";
          "XF86MonBrightnessUp" = "exec brightnessctl set 10%+";

          "XF86AudioMute" = "exec pamixer --toggle-mute";
          "XF86AudioLowerVolume" = "exec pamixer --decrease 10";
          "XF86AudioRaiseVolume" = "exec pamixer --increase 10";
        };

        input = {
          "type:touchpad" = {
            natural_scroll = "enabled";
            tap = "disabled";
            pointer_accel = "0.25";
          };

          "type:keyboard" = {
            xkb_options = "caps:escape";
            repeat_delay = "200";
          };
        };

        colors = {
          focused = {
            border = theme.normal.blue;
            background = theme.normal.blue;
            text = theme.normal.black;
            indicator = theme.normal.cyan;
            childBorder = theme.normal.blue;
          };

          focusedInactive = {
            border = theme.bright.black;
            background = theme.bright.black;
            text = theme.normal.white;
            indicator = theme.bright.black;
            childBorder = theme.bright.black;
          };

          unfocused = {
            border = theme.normal.black;
            background = theme.normal.black;
            text = theme.bright.black;
            indicator = theme.normal.black;
            childBorder = theme.normal.black;
          };

          urgent = {
            border = theme.normal.red;
            background = theme.normal.red;
            text = theme.normal.black;
            indicator = theme.normal.red;
            childBorder = theme.normal.red;
          };
        };

        bars = [
          {
            command = "${config.programs.waybar.package}/bin/waybar";
            position = "top";
          }
        ];

        window = {
          border = 0;
          commands = [
            {
              # Don't autolock while watching movies.
              criteria.class = "Firefox";
              command = "inhibit_idle fullscreen";
            }
          ];
        };
      };

      extraConfig = ''
        # Disable pinch-to-zoom. It never happens intentionally; only while scrolling.
        bindgesture pinch nop
        include /etc/sway/config.d/*
      '';
    };
  };
}
