{ config, lib, pkgs, ... }:

let cfg = config.presets.toolkits.networking.whois;

in with lib; {
  options.presets.toolkits.networking.whois.enable =
    mkEnableOption "Install and configure whois";

  config.programs.whois = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.whois;
  };
}
