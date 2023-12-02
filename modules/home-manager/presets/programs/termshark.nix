{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.termshark;

in {
  options.presets.termshark.enable =
    mkEnableOption "Install and configure termshark";

  config.programs.termshark = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.termshark;
  };
}
