{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.viddy;
in
{
  options.presets.programs.viddy.enable = mkEnableOption "Install Viddy";

  config.programs.viddy = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.viddy;
  };
}
