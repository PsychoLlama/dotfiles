{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.gammastep;
in

{
  options.psychollama.presets.services.gammastep.enable = lib.mkEnableOption "Use the gammastep blue light filter";

  config.services.gammastep = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.gammastep;
    dawnTime = "6:30-7:00";
    duskTime = "21:30-22:00";
  };
}
