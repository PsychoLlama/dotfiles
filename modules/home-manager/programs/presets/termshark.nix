{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs.presets.termshark;

in {
  options.programs.presets.termshark.enable =
    mkEnableOption "Install and configure termshark";

  config.programs.termshark = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.termshark;
  };
}
