{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.presets.dive;
in
{
  options.programs.presets.dive.enable = mkEnableOption "Install and configure dive";

  config.programs.dive = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dive;
  };
}
