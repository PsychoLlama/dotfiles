{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.toolkits.networking.termshark;

in {
  options.presets.toolkits.networking.termshark.enable =
    mkEnableOption "Install and configure termshark";

  config.programs.termshark = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.termshark;
  };
}
