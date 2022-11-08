{ config, lib, pkgs, ... }:

let cfg = config.programs.termshark;

in with lib; {
  options.programs.termshark = {
    enable = mkEnableOption "Whether to enable termshark";
    package = mkPackageOption pkgs "termshark" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
