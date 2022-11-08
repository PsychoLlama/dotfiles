{ config, lib, pkgs, ... }:

let cfg = config.presets.toolkits.networking.xh;

in with lib; {
  options.presets.toolkits.networking.xh.enable =
    mkEnableOption "Install and configure xh, a curl alternative";

  config.programs.xh = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.xh;
  };
}
