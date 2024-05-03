{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.presets.gammastep;
in
{
  options.services.presets.gammastep.enable = mkEnableOption "Use the gammastep blue light filter";

  config.services.gammastep = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.gammastep;
    dawnTime = "6:30-7:00";
    duskTime = "21:30-22:00";
  };
}
