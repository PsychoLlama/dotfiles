{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.sway;
  theme = config.theme.palette;

  # Export theme colors as sway variables.
  themeVars = ''
    set $normal_black ${theme.normal.black}
    set $normal_red ${theme.normal.red}
    set $normal_green ${theme.normal.green}
    set $normal_yellow ${theme.normal.yellow}
    set $normal_blue ${theme.normal.blue}
    set $normal_magenta ${theme.normal.magenta}
    set $normal_cyan ${theme.normal.cyan}
    set $normal_white ${theme.normal.white}
    set $bright_black ${theme.bright.black}
    set $bright_red ${theme.bright.red}
    set $bright_green ${theme.bright.green}
    set $bright_yellow ${theme.bright.yellow}
    set $bright_blue ${theme.bright.blue}
    set $bright_magenta ${theme.bright.magenta}
    set $bright_cyan ${theme.bright.cyan}
    set $bright_white ${theme.bright.white}
  '';
in

{
  options.psychollama.presets.programs.sway = {
    enable = lib.mkEnableOption "Use SwayWM as the desktop environment";
  };

  config = lib.mkIf cfg.enable {
    # Powers screen capture in Firefox.
    xdg.portal.wlr.enable = true;

    programs.sway = {
      enable = lib.mkDefault true;
      extraConfig = ''
        # theme.nix
        ${themeVars}

        # sway.conf
        ${builtins.readFile ./sway.conf}
      '';

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
    };
  };
}
