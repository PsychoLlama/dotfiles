{ config, lib, ... }:

let
  cfg = config.psychollama.presets.services.podman;
in
{
  options.psychollama.presets.services.podman = {
    enable = lib.mkEnableOption "Rootless container runtime using Podman";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman = {
      enable = lib.mkDefault true;

      # Create a `docker` alias for podman.
      dockerCompat = lib.mkDefault true;

      # Required for containers under podman-compose to be able to talk to
      # each other.
      defaultNetwork.settings.dns_enabled = lib.mkDefault true;
    };
  };
}
