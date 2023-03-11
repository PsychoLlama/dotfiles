{ config, lib, pkgs, ... }:

with lib;

let
  inherit (config.theme) palette;
  cfg = config.presets.swaylock;
  semi-black = "${palette.normal.black}d9"; # alpha(0.85)

in {
  options.presets.swaylock.enable =
    mkEnableOption "Give Swaylock an opinionated configuration";

  config.programs.swaylock.settings = mkIf cfg.enable {
    image = "attic/images/wallpapers/current";
    daemonize = true;
    show-failed-attempts = true;
    ignore-empty-password = true;

    color = palette.normal.black;
    line-color = palette.normal.black;

    ring-color = semi-black;
    inside-color = semi-black;
    text-color = palette.normal.red;
    key-hl-color = palette.normal.green;
    bs-hl-color = palette.normal.red;

    ring-clear-color = palette.bright.white;
    inside-clear-color = semi-black;
    text-clear-color = palette.bright.white;

    ring-ver-color = palette.normal.yellow;
    inside-ver-color = semi-black;
    text-ver-color = palette.normal.yellow;

    ring-wrong-color = palette.normal.red;
    inside-wrong-color = semi-black;
    text-wrong-color = palette.normal.red;
  };
}
