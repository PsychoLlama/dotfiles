{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.doggo;
in
{
  options.presets.programs.doggo.enable = mkEnableOption "Install and configure doggo, a DNS client";

  config.programs.doggo = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.doggo;
  };
}
