{ config, lib, ... }:

with lib;

let
  cfg = config.dotfiles.presets.desktop-environment;
in
{
  options.dotfiles.presets.desktop-environment.enable = mkEnableOption "Configure a desktop environment";

  config = mkIf cfg.enable {
    services.printing.enable = mkDefault true;

    # Powers screen capture in Firefox.
    xdg.portal.wlr.enable = true;

    # Used by Pipewire to get real-time thread priority.
    security.rtkit.enable = mkDefault true;

    services.pipewire = {
      enable = mkDefault true;
      audio.enable = mkDefault true;
      alsa.enable = mkDefault true;
      pulse.enable = mkDefault true;
    };

    programs.sway = {
      enable = mkDefault true;
      extraConfig = builtins.readFile ../../../config/sway.conf;

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
