{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.dive;
in
{
  options.presets.programs.dive.enable = mkEnableOption "Install and configure dive";

  config.programs.dive = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dive;
  };
}
