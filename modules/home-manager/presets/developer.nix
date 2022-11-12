{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.developer;

in {
  options.presets.developer.enable = mkEnableOption "Enable OS developer tools";

  config.home.packages =
    mkIf (cfg.enable && pkgs.stdenv.isLinux) [ pkgs.man-pages ];
}
