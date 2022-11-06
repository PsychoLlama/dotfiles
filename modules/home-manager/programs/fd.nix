{ config, lib, pkgs, ... }:

let cfg = config.programs.fd;

in with lib; {
  options.programs.fd.enable = mkEnableOption "Whether to enable fd";
  config.home.packages = mkIf cfg.enable [ pkgs.fd ];
}
