{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.nmap;

in {
  options.presets.nmap.enable =
    mkEnableOption "Install and configure nmap";

  config.programs.nmap = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.nmap;
  };
}
