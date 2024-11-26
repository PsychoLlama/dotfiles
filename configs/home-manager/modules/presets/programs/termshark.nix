{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.termshark;
in
{
  options.presets.programs.termshark.enable = mkEnableOption "Install and configure termshark";

  config.programs.termshark = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.termshark;
  };
}
