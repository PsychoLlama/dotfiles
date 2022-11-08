{ config, lib, ... }:

let cfg = config.presets.toolkits.networking;

in with lib; {
  imports = [ ./dogdns.nix ./nmap.nix ./termshark.nix ./whois.nix ./xh.nix ];

  options.presets.toolkits.networking.enable =
    mkEnableOption "Use an opinionated networking toolkit";

  config.presets.toolkits.networking = mkIf cfg.enable {
    dogdns.enable = mkDefault true;
    nmap.enable = mkDefault true;
    termshark.enable = mkDefault true;
    whois.enable = mkDefault true;
    xh.enable = mkDefault true;
  };
}
