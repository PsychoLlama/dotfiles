{ config, lib, pkgs, ... }:

let cfg = config.presets.toolkits.networking.termshark;

in with lib; {
  options.presets.toolkits.networking.termshark.enable =
    mkEnableOption "Install and configure termshark";

  config.programs.termshark = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.termshark;
  };
}
