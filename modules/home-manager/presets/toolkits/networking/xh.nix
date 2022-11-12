{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.toolkits.networking.xh;

in {
  options.presets.toolkits.networking.xh.enable =
    mkEnableOption "Install and configure xh, a curl alternative";

  config.programs.xh = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.xh;
  };
}
