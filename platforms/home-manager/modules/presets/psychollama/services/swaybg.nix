{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.presets.services.swaybg;
in

{
  options.presets.services.swaybg.enable = lib.mkEnableOption "Manage wallpapers with swaybg";

  config.services.swaybg = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.swaybg;
    image = "attic/images/wallpapers/current";
  };
}
