{ config, lib, pkgs, ... }:

let cfg = config.programs.xh;

in with lib; {
  options.programs.xh = {
    enable = mkEnableOption "Whether to enable xh, a curl alternative";
    package = mkPackageOption pkgs "xh" { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.package ];
}
