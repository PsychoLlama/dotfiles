{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.direnv;

in {
  options.presets.direnv.enable = mkEnableOption "Install and configure direnv";
  config.programs.direnv = mkIf cfg.enable { enable = true; };
}