{ config, lib, pkgs, ... }:

with lib;

let cfg = config.dotfiles.presets.desktop-environment;

in {
  options.dotfiles.presets.desktop-environment.enable =
    mkEnableOption "Configure a desktop environment";

  config = mkIf cfg.enable {
    hardware.pulseaudio.enable = mkDefault true;
    services.printing.enable = mkDefault true;
    sound.enable = mkDefault true;

    programs.sway = {
      enable = mkDefault true;
      extraConfig = builtins.readFile ../../../config/sway.conf;

      input = {
        "type:touchpad".natural_scroll = "enabled";
        "type:keyboard" = {
          xkb_options = "caps:escape";
          repeat_delay = "200";
        };
      };
    };
  };
}
