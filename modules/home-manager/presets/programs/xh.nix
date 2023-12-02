{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.xh;

in {
  options.presets.xh.enable =
    mkEnableOption "Install and configure xh, a curl alternative";

  config.programs.xh = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.xh;
  };
}
