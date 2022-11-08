{ config, lib, pkgs, ... }:

let cfg = config.programs.nmap;

in with lib; {
  options.programs.nmap = {
    enable = mkEnableOption "Whether to enable nmap";
    package = mkPackageOption pkgs "nmap" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
