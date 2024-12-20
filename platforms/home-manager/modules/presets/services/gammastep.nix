{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.services.gammastep;
in
{
  options.presets.services.gammastep.enable = mkEnableOption "Use the gammastep blue light filter";

  config.services.gammastep = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.gammastep;
    dawnTime = "6:30-7:00";
    duskTime = "21:30-22:00";
  };
}
