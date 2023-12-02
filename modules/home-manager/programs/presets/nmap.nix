{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs.presets.nmap;

in {
  options.programs.presets.nmap.enable =
    mkEnableOption "Install and configure nmap";

  config.programs.nmap = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.nmap;
  };
}
