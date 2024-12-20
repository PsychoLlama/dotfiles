{ pkgs, lib, ... }:

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
    systemPackages = with pkgs.unstable; [ borgbackup ];
    variables.BORG_REPO = "/mnt/borg";
  };

  profiles = {
    full.enable = true;
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

    profiles = {
      full.enable = true;
      linux-desktop.enable = true;
    };

    presets.fonts.enable = true;
  };

  services = {
    automatic-timezoned.enable = true;

    zfs = {
      trim.enable = true;
      autoScrub.enable = true;
      autoSnapshot = {
        enable = true;
        flags = "-k -p --utc";
      };
    };

    syncthing = {
      enable = true;
      package = pkgs.unstable.syncthing;

      user = "overlord";
      group = "users";
      dataDir = "/home/overlord";

      settings = {
        options.urAccepted = 3;
        gui.theme = "dark";

        # A general-purpose box for reliable storage.
        folders."/home/overlord/attic" = {
          id = "attic";
          devices = [
            "file-server"
            "phone"
          ];
          label = "Attic";
        };

        devices = {
          file-server = {
            addresses = [ "tcp://rpi3-002.host.selfhosted.city" ];
            id = "MLM3RUS-6LHM76Q-OPW5UIC-EAH7EUM-ZNG6TJW-TDASURZ-GCZ2YOX-ASNI6Q4";
          };

          phone = {
            addresses = [ "dynamic" ];
            id = "S2U7KKV-SXJGOI3-6MSJWIT-U2JP32Y-HH7WZU5-ZDS6KAT-6CNYRAM-ZQTWZAQ";
          };
        };
      };
    };

    # fprintd doesn't play well with swaylock's pam module. It effectively
    # disables password input.
    fprintd.enable = lib.mkForce false;
  };

  programs = {
    wireshark.enable = true;

    sway.input = {
      "type:touchpad" = {
        natural_scroll = "enabled";
        pointer_accel = "0.25";
      };

      "type:keyboard" = {
        xkb_options = "caps:escape";
        repeat_delay = "200";
      };
    };

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

  virtualisation.podman = {
    enable = true;
    autoPrune.enable = true;
  };

  fonts.enableDefaultPackages = true;
  hardware.bluetooth.enable = true;
  system.stateVersion = "20.09";
}
