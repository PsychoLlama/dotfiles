{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.dogdns;

in {
  options.presets.dogdns.enable =
    mkEnableOption "Install and configure dog, a DNS client";

  config.programs.dogdns = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dogdns;
  };
}
