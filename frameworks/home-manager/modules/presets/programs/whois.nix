{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.whois;
in
{
  options.presets.programs.whois.enable = mkEnableOption "Install and configure whois";

  config.programs.whois = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.whois;
  };
}
