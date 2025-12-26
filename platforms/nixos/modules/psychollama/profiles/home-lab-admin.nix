{ config, lib, ... }:

let
  inherit (config.psychollama.settings) username;
  cfg = config.psychollama.profiles.home-lab-admin;

  hosts = {
    rpi4-001 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAyb4vh9xDEEV+30G0UPMTSdtVq3Tyfgl9I9VRwf226v";
    rpi4-002 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJLMZ6+HaPahE4gGIAWW/uGIl/y40p/rSfIhb5t4G+g9";
    rpi4-003 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFsNbo3bbm0G11GAbRwnr944AitRyqoQMN4LG7rMsvpK";
    rpi3-001 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN2VZGgphnMAD5tLG+IHBlBWdlUPNfvYEMDK8OQCrG/A";
    rpi3-002 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKrGfslz9RlB2EzrTL3SfO/NZB5fPiVXWkK+aQRZrlel";
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
        Host ${lib.concatStringsSep " " (lib.attrNames hosts)}
          User root
      '';

      knownHosts = lib.mapAttrs (hostName: publicKey: { inherit publicKey; }) hosts;
    };
  };
}
