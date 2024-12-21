{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.miniserve;
in
{
  options.presets.programs.miniserve.enable = mkEnableOption "Install and configure miniserve";

  config.programs.miniserve = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.miniserve;
  };
}
