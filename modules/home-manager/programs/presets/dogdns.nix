{ config, lib, pkgs, ... }:

with lib;

let cfg = config.programs.presets.dogdns;

in {
  options.programs.presets.dogdns.enable =
    mkEnableOption "Install and configure dog, a DNS client";

  config.programs.dogdns = mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.dogdns;
  };
}
