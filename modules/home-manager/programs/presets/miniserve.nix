{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs.presets.miniserve;

in {
  options.programs.presets.miniserve.enable =
    mkEnableOption "Install and configure miniserve";

  config.programs.miniserve = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.miniserve;
  };
}
