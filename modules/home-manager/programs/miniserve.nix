{ config, lib, pkgs, ... }:

let cfg = config.programs.miniserve;

in with lib; {
  options.programs.miniserve = {
    enable = mkEnableOption "Whether to enable miniserve";
    package = mkPackageOption pkgs "miniserve" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
