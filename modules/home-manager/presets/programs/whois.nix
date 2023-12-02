{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.whois;

in {
  options.presets.whois.enable =
    mkEnableOption "Install and configure whois";

  config.programs.whois = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.whois;
  };
}
