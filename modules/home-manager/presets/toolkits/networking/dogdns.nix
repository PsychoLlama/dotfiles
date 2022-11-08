{ config, lib, pkgs, ... }:

let cfg = config.presets.toolkits.networking.dogdns;

in with lib; {
  options.presets.toolkits.networking.dogdns.enable =
    mkEnableOption "Install and configure dog, a DNS client";

  config.programs.dogdns = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dogdns;
  };
}
