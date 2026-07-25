{ lib, ... }:

let
  keyPath = "/etc/ssh/ssh_host_agenix_key";
in

{
  platforms.nixos = {
    services.openssh = {
      enable = true;
      openFirewall = false;

      # Generate a dedicated host key for agenix. Ordered last because sshd
      # presents the *first* key of a given type, and other modules contribute
      # ed25519 keys too — this one exists to decrypt secrets, not to define
      # the machine's SSH identity.
      hostKeys = lib.mkAfter [
        {
          type = "ed25519";
          path = keyPath;
          comment = "agenix";
        }
      ];
    };

    # Point agenix to the key. `age.*` is declared by the agenix module at the
    # assembly site, so it is already in the NixOS fixpoint this merges into.
    age.identityPaths = [ keyPath ];
  };
}
