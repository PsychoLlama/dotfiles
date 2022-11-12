{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.toolkits.networking.whois;

in {
  options.presets.toolkits.networking.whois.enable =
    mkEnableOption "Install and configure whois";

  config.programs.whois = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.whois;
  };
}
