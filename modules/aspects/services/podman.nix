{ config, ... }:

# Bound out here because the module below shadows `config` with its own.
let
  inherit (config.identity) username;
in

{
  imports = [ ../../rhizome/identity.nix ];

  exports.nixos =
    { config, lib, ... }:

    {
      # Podman creates the group; joining it is what makes the socket usable.
      users.users.${username}.extraGroups = [ "podman" ];

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
