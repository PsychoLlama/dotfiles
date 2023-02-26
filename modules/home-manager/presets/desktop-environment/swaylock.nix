{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.swaylock;

in {
  options.presets.swaylock.enable =
    mkEnableOption "Give Swaylock an opinionated configuration";

  config.programs.swaylock.settings = mkIf cfg.enable {
    image = "attic/images/wallpapers/current";
    color = "000000";
    daemonize = true;
    show-failed-attempts = true;
    ignore-empty-password = true;
  };
}
