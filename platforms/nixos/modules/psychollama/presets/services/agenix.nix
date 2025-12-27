{
  config,
  lib,
  ...
}:

let
  cfg = config.psychollama.presets.services.agenix;
  keyPath = "/etc/ssh/ssh_host_agenix_key";
in

{
  options.psychollama.presets.services.agenix = {
    enable = lib.mkEnableOption "Agenix secrets decryption";
  };

  config = lib.mkIf cfg.enable {
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
}
