{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.presets.doggo;
in
{
  options.programs.presets.doggo.enable = mkEnableOption "Install and configure doggo, a DNS client";

  config.programs.doggo = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.doggo;
  };
}
