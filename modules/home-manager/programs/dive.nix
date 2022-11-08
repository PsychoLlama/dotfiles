{ config, lib, pkgs, ... }:

let cfg = config.programs.dive;

in with lib; {
  options.programs.dive = {
    enable = mkEnableOption "Whether to install dive";
    package = mkPackageOption pkgs "dive" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
