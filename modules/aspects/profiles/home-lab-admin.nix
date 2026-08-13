{ config, ... }:

# Bound out here because the module below shadows `config` with its own.
let
  inherit (config.identity) username;
in

{
  imports = [ ../../rhizome/identity.nix ];

  exports.nixos =
    { config, lib, ... }:

    # Configure the machine as an admin to the home lab.
    # See: https://github.com/PsychoLlama/home-lab/

    let
      hosts = {
        nas-001 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOx6MIH8pVfBi0dckuIgssJO5JzlnEKrJrhNSPs7giTR";
        rpi4-001 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAyb4vh9xDEEV+30G0UPMTSdtVq3Tyfgl9I9VRwf226v";
        rpi4-002 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJLMZ6+HaPahE4gGIAWW/uGIl/y40p/rSfIhb5t4G+g9";
        rpi4-003 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFsNbo3bbm0G11GAbRwnr944AitRyqoQMN4LG7rMsvpK";
      };
    in

    {
      nix.settings = {
        trusted-users = [ username ]; # Needed by `colmena`.
        builders-use-substitutes = true;
      };

      # sshd serves the first host key of each type, and the agenix preset
      # registers an ed25519 key too. Ordering by file layout would silently
      # swap which key clients see.
      services.openssh.hostKeys = lib.mkBefore [
        {
          type = "ed25519";
          path = "/root/.ssh/home_lab";
          comment = "Home Lab deploy key";
        }
      ];

      programs.ssh = {
        extraConfig = ''
          # Tailscale
          Host ${lib.concatStringsSep " " (lib.attrNames hosts)}
            User root

          # UniFi U6-Lite
          Host access-point.host.nova.selfhosted.city
            User admin

          # LAN lookup
          Host *.host.nova.selfhosted.city
            User root
        '';

        knownHosts = lib.mapAttrs (hostName: publicKey: { inherit publicKey; }) hosts;
      };
    };
}
