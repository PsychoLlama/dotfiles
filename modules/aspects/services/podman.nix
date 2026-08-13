{
  exports.nixos =
    { host, lib, ... }:

    let
      inherit (host.identity) username;
    in

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
