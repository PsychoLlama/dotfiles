{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.toolkits.networking.nmap;

in {
  options.presets.toolkits.networking.nmap.enable =
    mkEnableOption "Install and configure nmap";

  config.programs.nmap = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.nmap;
  };
}
