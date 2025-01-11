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

  psychollama = {
    presets.services.syncthing.username = username;

    profiles = {
      home-lab-admin.enable = true;
      full.enable = true;
    };
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

  hardware.bluetooth.enable = true;
  system.stateVersion = "20.09";
}
