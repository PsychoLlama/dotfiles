{ lib, pkgs, ... }:

let
  username = "overlord";
in

{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    initrd.supportedFilesystems = [ "zfs" ];
    supportedFilesystems = [ "zfs" ];
    kernelPackages = pkgs.linuxPackages_6_6; # Latest compatible with ZFS.

    # ZFS doesn't support freeze/thaw APIs. Hibernation could corrupt files.
    # https://github.com/openzfs/zfs/issues/260
    #
    # Also, set a maximum size on the ZFS Adaptive Replacement Cache (1GB).
    kernelParams = [
      "nohibernate"
      "zfs.zfs_arc_max=1073741824"
    ];

    loader = {
      # Fixes a potential issue where too many hardlinks in the nix store can
      # brick the boot process.
      grub.copyKernels = true;

      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  # Network configuration.
  networking = {
    networkmanager.enable = true;
    interfaces.wlp0s20f3.useDHCP = true;
    useDHCP = false; # Deprecated option - explicitly opt out.
    hostId = "daf96cd8";
  };

  nix = {
    settings = {
      trusted-users = [ username ];
      builders-use-substitutes = true;
    };

    # Prune older entries to avoid running out of space on the boot partition.
    gc = {
      automatic = true;
      options = "--delete-older-than 180d";
    };
  };

  services.openssh.hostKeys = [
    {
      type = "ed25519";
      path = "/root/.ssh/home_lab";
      comment = "Home Lab deploy key";
    }
  ];

  environment = {
    systemPackages = [ pkgs.unstable.borgbackup ];
    variables.BORG_REPO = "/mnt/borg";
  };

  psychollama = {
    presets.services.syncthing.username = username;
    profiles.full.enable = true;
  };

  users.users.${username} = {
    isNormalUser = true;
    name = "overlord";
    description = "Jesse Gibson";
    extraGroups = [
      "networkmanager"
      "podman"
      "wheel"
    ];
  };

  home-manager.users.${username} = {
    home.stateVersion = "22.05";
    home.packages = [ pkgs.man-pages ];

    programs.git = {
      userName = "Jesse Gibson";
      userEmail = "JesseTheGibson@gmail.com";
    };

    psychollama.profiles = {
      full.enable = true;
      linux-desktop.enable = true;
    };
  };

  services = {
    zfs = {
      trim.enable = true;
      autoScrub.enable = true;
      autoSnapshot = {
        enable = true;
        flags = "-k -p --utc";
      };
    };

    # fprintd doesn't play well with swaylock's pam module. It effectively
    # disables password input.
    fprintd.enable = lib.mkForce false;
  };

  programs = {
    ssh = {
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
  };

  hardware.bluetooth.enable = true;
  system.stateVersion = "20.09";
}
