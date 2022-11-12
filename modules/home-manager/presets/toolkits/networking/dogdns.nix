{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.toolkits.networking.dogdns;

in {
  options.presets.toolkits.networking.dogdns.enable =
    mkEnableOption "Install and configure dog, a DNS client";

  config.programs.dogdns = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dogdns;
  };
}
