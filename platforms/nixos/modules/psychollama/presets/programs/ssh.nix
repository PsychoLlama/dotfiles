{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.ssh;
in

{
  # This isn't quite a generalizable config. It assumes I'm configuring SSH
  # for my home lab, which may not be true of all linux workstations.
  options.psychollama.presets.programs.ssh = {
    enable = lib.mkEnableOption "Configure SSH to work with the home lab";
  };

  config.programs.ssh = lib.mkIf cfg.enable {
    extraConfig = ''
      CanonicalizeHostname yes
      CanonicalDomains host.selfhosted.city
      CanonicalizeMaxDots 0

      Host *.host.selfhosted.city
      User root
    '';

    knownHosts =
      lib.mapAttrs'
        (hostName: publicKey: lib.nameValuePair "${hostName}.host.selfhosted.city" { inherit publicKey; })
        {
          rpi4-001 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAyb4vh9xDEEV+30G0UPMTSdtVq3Tyfgl9I9VRwf226v";
          rpi4-002 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJLMZ6+HaPahE4gGIAWW/uGIl/y40p/rSfIhb5t4G+g9";
          rpi4-003 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFsNbo3bbm0G11GAbRwnr944AitRyqoQMN4LG7rMsvpK";
          rpi3-001 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN2VZGgphnMAD5tLG+IHBlBWdlUPNfvYEMDK8OQCrG/A";
          rpi3-002 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKrGfslz9RlB2EzrTL3SfO/NZB5fPiVXWkK+aQRZrlel";
        };
  };
}
