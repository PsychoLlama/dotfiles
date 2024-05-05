{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.presets.viddy;
in
{
  options.programs.presets.viddy.enable = mkEnableOption "Install Viddy";

  config.programs.viddy = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.viddy;
  };
}
