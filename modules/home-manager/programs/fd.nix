{ config, lib, pkgs, ... }:

let cfg = config.programs.fd;

in with lib; {
  options.programs.fd = {
    enable = mkEnableOption "Whether to enable fd";
    package = mkPackageOption pkgs "fd" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
