{ config, lib, ... }:

let
  cfg = config.psychollama.presets.services.tailscale;
in

{
  options.psychollama.presets.services.tailscale = {
    enable = lib.mkEnableOption "Connect to Tailscale VPN";
  };

  config.services.tailscale = lib.mkIf cfg.enable {
    enable = true;
    extraUpFlags = [ "--advertise-tags=tag:laptop" ];
  };
}
