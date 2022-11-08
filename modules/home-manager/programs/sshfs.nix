{ config, lib, pkgs, ... }:

let cfg = config.programs.sshfs;

in with lib; {
  options.programs.sshfs = {
    enable = mkEnableOption "Whether to enable sshfs";
    package = mkPackageOption pkgs "sshfs" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
