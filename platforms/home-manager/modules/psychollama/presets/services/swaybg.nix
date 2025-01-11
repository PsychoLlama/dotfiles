{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.swaybg;
in

{
  options.psychollama.presets.services.swaybg.enable = lib.mkEnableOption "Manage wallpapers with swaybg";

  config.services.swaybg = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.swaybg;
    image = "attic/images/wallpapers/current";
  };
}
