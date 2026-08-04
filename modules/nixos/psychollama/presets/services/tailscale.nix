{ config, lib, ... }:

let
  cfg = config.psychollama.presets.services.tailscale;
in

{
  options.psychollama.presets.services.tailscale = {
    enable = lib.mkEnableOption "Connect to Tailscale VPN";
  };

  config = lib.mkIf cfg.enable {
    services.tailscale = {
      enable = true;
      extraUpFlags = [ "--advertise-tags=tag:laptop" ];
    };

    # Use systemd-resolved for DNS. Tailscale integrates with it to provide
    # split DNS (MagicDNS for tailnet, system DNS for everything else). This
    # also fixes DNS breaking after suspend/resume cycles.
    services.resolved.enable = true;
    networking.networkmanager.dns = "systemd-resolved";
  };
}
