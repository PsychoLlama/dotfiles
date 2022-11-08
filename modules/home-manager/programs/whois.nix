{ config, lib, pkgs, ... }:

let cfg = config.programs.whois;

in with lib; {
  options.programs.whois = {
    enable = mkEnableOption "Whether to enable whois";
    package = mkPackageOption pkgs "whois" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
