{
  exports.nixos =
    {
      config,
      lib,
      ...
    }:

    let
      keyPath = "/etc/ssh/ssh_host_agenix_key";
    in

    {
      config = {
        services.openssh = {
          enable = true;
          openFirewall = false;

          # Generate a dedicated host key for agenix.
          hostKeys = [
            {
              type = "ed25519";
              path = keyPath;
              comment = "agenix";
            }
          ];
        };

        # Point agenix to the key.
        age.identityPaths = [ keyPath ];
      };
    };
}
