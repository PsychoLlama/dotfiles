{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.swaybg;

in {
  options.presets.swaybg.enable =
    mkEnableOption "Manage wallpapers with swaybg";

  config.services.swaybg = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.swaybg;
    image = "attic/images/wallpapers/current";
  };
}
