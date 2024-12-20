{ config, lib, ... }:

let
  cfg = config.presets.programs.sway;
in
{
  options.presets.programs.sway = {
    enable = lib.mkEnableOption "Use SwayWM as the desktop environment";
  };

  config = lib.mkIf cfg.enable {
    # Powers screen capture in Firefox.
    xdg.portal.wlr.enable = true;

    programs.sway = {
      enable = lib.mkDefault true;
      extraConfig = builtins.readFile ./sway.conf;

      input = {
        "type:touchpad" = {
          natural_scroll = "enabled";
          tap = "disabled";
        };

        "type:keyboard" = {
          xkb_options = "caps:escape";
          repeat_delay = "200";
        };
      };
    };
  };
}
