{ config, lib, ... }:

let
  inherit (config.psychollama.settings) username;
  cfg = config.psychollama.profiles.home-lab-admin;

  hosts = {
    nas-001 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOx6MIH8pVfBi0dckuIgssJO5JzlnEKrJrhNSPs7giTR";
    rpi4-001 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAyb4vh9xDEEV+30G0UPMTSdtVq3Tyfgl9I9VRwf226v";
    rpi4-002 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJLMZ6+HaPahE4gGIAWW/uGIl/y40p/rSfIhb5t4G+g9";
    rpi4-003 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFsNbo3bbm0G11GAbRwnr944AitRyqoQMN4LG7rMsvpK";
  };
in

{
  options.psychollama.profiles.home-lab-admin = {
    enable = lib.mkEnableOption ''
      Configure the machine as an admin to the home lab.
      See: https://github.com/PsychoLlama/home-lab/
    '';
  };

  config = lib.mkIf cfg.enable {
    nix.settings = {
      trusted-users = [ username ]; # Needed by `colmena`.
      builders-use-substitutes = true;
    };

    services.openssh.hostKeys = [
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
