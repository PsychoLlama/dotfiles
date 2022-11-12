{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.miniserve;

in {
  options.presets.miniserve.enable =
    mkEnableOption "Install and configure miniserve";

  config.programs.miniserve = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.miniserve;
  };
}
