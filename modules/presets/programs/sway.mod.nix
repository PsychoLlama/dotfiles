{
  self,
  lib,
  ...
}:

# SwayWM: the NixOS half enables the compositor and its portal, the
# home-manager half writes the config. Two platforms, one module.
let
  inherit (self.theme) palette;
in

{
  platforms.nixos = {
    programs.sway.enable = lib.mkDefault true;

    # Home-manager generates the real config; suppress the package default.
    environment.etc."sway/config".enable = false;

    # Powers screen capture in Firefox.
    xdg.portal.wlr.enable = true;
  };

  platforms.home-manager =
    { config, ... }:

    let
      swaylock = lib.getExe' config.programs.swaylock.package "swaylock";
      wezterm = lib.getExe' config.programs.wezterm.package "wezterm";
      fuzzel = lib.getExe' config.programs.fuzzel.package "fuzzel";
      bemoji = lib.getExe' config.programs.bemoji.package "bemoji";
      grim = lib.getExe' config.programs.grim.package "grim";
      slurp = lib.getExe' config.programs.slurp.package "slurp";
      wlCopy = lib.getExe' config.programs."wl-clipboard".package "wl-copy";
      playerctl = lib.getExe' config.programs.playerctl.package "playerctl";
      brightnessctl = lib.getExe' config.programs.brightnessctl.package "brightnessctl";
      pamixer = lib.getExe' config.programs.pamixer.package "pamixer";
      waybar = lib.getExe' config.programs.waybar.package "waybar";

      # Laptop built-in keyboard. Find identifiers with `swaymsg -t get_inputs`.
      thinkpadKeyboard = "1:1:AT_Translated_Set_2_keyboard";

      # Shared by window titles and the bar. Matches waybar's CSS font family.
      font = {
        names = [ "Fira Code" ];
        size = 10.0;
      };
    in

    {
      wayland.windowManager.sway = {
        enable = true;
        package = null;
        checkConfig = false;
        systemd.enable = true;

        config = rec {
          fonts = font;

          modifier = "Mod4";
          terminal = wezterm;
          menu = fuzzel;
          modes = { };

          keybindings = {
            # Lock the computer.
            "${modifier}+Control+q" = "exec ${swaylock}";
            "${modifier}+q" = "kill";

            "${modifier}+Return" = "exec ${wezterm}";
            "${modifier}+space" = "exec ${fuzzel}";

            # Emoji picker.
            "${modifier}+period" = "exec ${bemoji} -t";

            # Screenshot: select region, save to ~/screenshots/, copy to clipboard.
            Print = ''exec ${grim} -g "$(${slurp})" - | tee ~/screenshots/"$(date --iso-8601=seconds).png" | ${wlCopy}'';

            "${modifier}+Shift+r" = "reload";

            # Focus.
            "${modifier}+h" = "focus left";
            "${modifier}+j" = "focus down";
            "${modifier}+k" = "focus up";
            "${modifier}+l" = "focus right";

            # Move windows.
            "${modifier}+Shift+h" = "move left";
            "${modifier}+Shift+j" = "move down";
            "${modifier}+Shift+k" = "move up";
            "${modifier}+Shift+l" = "move right";

            # Workspaces.
            "${modifier}+0" = "workspace number 0";
            "${modifier}+1" = "workspace number 1";
            "${modifier}+2" = "workspace number 2";
            "${modifier}+3" = "workspace number 3";
            "${modifier}+4" = "workspace number 4";
            "${modifier}+5" = "workspace number 5";
            "${modifier}+6" = "workspace number 6";
            "${modifier}+7" = "workspace number 7";
            "${modifier}+8" = "workspace number 8";
            "${modifier}+9" = "workspace number 9";

            "${modifier}+Tab" = "workspace next";
            "${modifier}+Shift+Tab" = "workspace prev";

            # Move focused container to workspace.
            "${modifier}+Shift+0" = "move container to workspace number 0; workspace number 0";
            "${modifier}+Shift+1" = "move container to workspace number 1; workspace number 1";
            "${modifier}+Shift+2" = "move container to workspace number 2; workspace number 2";
            "${modifier}+Shift+3" = "move container to workspace number 3; workspace number 3";
            "${modifier}+Shift+4" = "move container to workspace number 4; workspace number 4";
            "${modifier}+Shift+5" = "move container to workspace number 5; workspace number 5";
            "${modifier}+Shift+6" = "move container to workspace number 6; workspace number 6";
            "${modifier}+Shift+7" = "move container to workspace number 7; workspace number 7";
            "${modifier}+Shift+8" = "move container to workspace number 8; workspace number 8";
            "${modifier}+Shift+9" = "move container to workspace number 9; workspace number 9";

            # Layout.
            "${modifier}+s" = "layout stacking";
            "${modifier}+w" = "layout tabbed";
            "${modifier}+e" = "layout toggle split";
            "${modifier}+f" = "fullscreen";

            # Playback control on the laptop's built-in keyboard only. External keyboards
            # have their own Home/End/Insert that shouldn't be hijacked.
            "--input-device=${thinkpadKeyboard} Insert" = "exec ${playerctl} play-pause";
            "--input-device=${thinkpadKeyboard} Home" = "exec ${playerctl} previous";
            "--input-device=${thinkpadKeyboard} End" = "exec ${playerctl} next";

            XF86AudioPlay = "exec ${playerctl} play-pause";
            XF86AudioPrev = "exec ${playerctl} previous";
            XF86AudioNext = "exec ${playerctl} next";

            XF86MonBrightnessDown = "exec ${brightnessctl} set 10%-";
            XF86MonBrightnessUp = "exec ${brightnessctl} set 10%+";

            XF86AudioMute = "exec ${pamixer} --toggle-mute";
            XF86AudioLowerVolume = "exec ${pamixer} --decrease 10";
            XF86AudioRaiseVolume = "exec ${pamixer} --increase 10";
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
              border = palette.normal.blue;
              background = palette.normal.blue;
              text = palette.normal.black;
              indicator = palette.normal.cyan;
              childBorder = palette.normal.blue;
            };

            focusedInactive = {
              border = palette.bright.black;
              background = palette.bright.black;
              text = palette.normal.white;
              indicator = palette.bright.black;
              childBorder = palette.bright.black;
            };

            unfocused = {
              border = palette.normal.black;
              background = palette.normal.black;
              text = palette.bright.black;
              indicator = palette.normal.black;
              childBorder = palette.normal.black;
            };

            urgent = {
              border = palette.normal.red;
              background = palette.normal.red;
              text = palette.normal.black;
              indicator = palette.normal.red;
              childBorder = palette.normal.red;
            };
          };

          bars = [
            {
              command = waybar;
              position = "top";
              fonts = font;
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
