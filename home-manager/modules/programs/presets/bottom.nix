{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.presets.bottom;
in
{
  options.programs.presets.bottom.enable = mkEnableOption "Use and configure the bottom process monitor";

  config.programs.bottom = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.bottom;
    settings.flags.temperature_type = "f";
  };
}
