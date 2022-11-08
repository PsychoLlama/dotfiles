{ config, lib, pkgs, ... }:

let cfg = config.presets.miniserve;

in with lib; {
  options.presets.miniserve.enable =
    mkEnableOption "Install and configure miniserve";

  config.programs.miniserve = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.miniserve;
  };
}
