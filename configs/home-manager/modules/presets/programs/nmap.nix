{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.nmap;
in
{
  options.presets.programs.nmap.enable = mkEnableOption "Install and configure nmap";

  config.programs.nmap = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.nmap;
  };
}
