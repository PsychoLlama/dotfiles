{ config, lib, pkgs, ... }:

let cfg = config.programs.dogdns;

in with lib; {
  options.programs.dogdns = {
    enable = mkEnableOption "Whether to enable dog, a DNS client";
    package = mkPackageOption pkgs "dogdns" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
