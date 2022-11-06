{ config, lib, pkgs, ... }:

let cfg = config.presets.dunst;

in with lib; {
  options.presets.dunst.enable =
    mkEnableOption "Use the dunst notification daemon";

  config.services.dunst = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dunst;
    configFile = ../../../../config/dunst.cfg;
  };
}
