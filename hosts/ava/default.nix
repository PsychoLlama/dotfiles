{ config, pkgs, lib, inputs, system, ... }:

{
  imports = [ ./hardware-configuration.nix ../common/linux.nix ];

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    initrd.supportedFilesystems = [ "zfs" ];
    supportedFilesystems = [ "zfs" ];
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

    # ZFS doesn't support freeze/thaw APIs. Hibernation could corrupt files.
    # https://github.com/openzfs/zfs/issues/260
    #
    # Also, set a maximum size on the ZFS Adaptive Replacement Cache (1GB).
    kernelParams = [ "nohibernate" "zfs.zfs_arc_max=1073741824" ];

    # Fixes a potential issue where too many hardlinks in the nix store can
    # brick the boot process.
    loader.grub.copyKernels = true;
  };

  # Network configuration.
  networking = {
    interfaces.wlp0s20f3.useDHCP = true;
    useDHCP = false; # Deprecated option - explicitly opt out.
    hostId = "daf96cd8";
  };

  nix.settings = {
    trusted-users = [ config.dotfiles.user.name ];
    builders-use-substitutes = true;
  };

  services.openssh.hostKeys = [{
    type = "ed25519";
    path = "/root/.ssh/nixops_deploy";
    comment = "NixOps deploy key";
  }];

  environment = {
    systemPackages = with pkgs.unstable; [ borgbackup radare2 ];
    variables.BORG_REPO = "/mnt/borg";
  };

  dotfiles = {
    presets.kitchen-sink.enable = true;
    packageSet = "nixpkgs-unstable";

    user = {
      name = "overlord";
      description = "Jesse Gibson";
      extraGroups = [ "docker" ];
    };
  };

  home-manager.users.${config.dotfiles.user.name} = {
    home.stateVersion = "22.05";

    programs.git = {
      userName = "Jesse Gibson";
      userEmail = "JesseTheGibson@gmail.com";
    };

    presets = {
      desktop-environment.enable = true;
      developer.enable = true;
      fonts.enable = true;
      terminal-environment.enable = true;
    };
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

      # A general-purpose box for reliable storage.
      folders."/home/overlord/attic" = {
        id = "attic";
        devices = [ "file-server" "phone" ];
        label = "Attic";
      };

      devices = {
        file-server = {
          addresses = [ "tcp://hactar.host.selfhosted.city" ];
          id =
            "YOTWLUU-HU6VAII-DJFNJCT-QBVRGD7-EM37VVL-WNB2HFW-5YFWCUM-INTE4AI";
        };

        phone = {
          addresses = [ "dynamic" ];
          id =
            "S2U7KKV-SXJGOI3-6MSJWIT-U2JP32Y-HH7WZU5-ZDS6KAT-6CNYRAM-ZQTWZAQ";
        };
      };

      extraOptions = {
        options.urAccepted = 3;
        gui.theme = "dark";
      };
    };

    # fprintd doesn't play well with swaylock's pam module. It effectively
    # disables password input.
    fprintd.enable = lib.mkForce false;
  };

  programs = {
    wireshark.enable = true;

    sway.input = {
      "type:touchpad".natural_scroll = "enabled";
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
        User admin
      '';

      knownHosts = let hostNames = host: [ "${host}.host.selfhosted.city" ];
      in {
        tron = {
          hostNames = hostNames "tron";
          publicKey = ''
            ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFsNbo3bbm0G11GAbRwnr944AitRyqoQMN4LG7rMsvpK
          '';
        };

        clu = {
          hostNames = hostNames "clu";
          publicKey = ''
            ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAyb4vh9xDEEV+30G0UPMTSdtVq3Tyfgl9I9VRwf226v
          '';
        };

        glados = {
          hostNames = hostNames "glados";
          publicKey = ''
            ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJLMZ6+HaPahE4gGIAWW/uGIl/y40p/rSfIhb5t4G+g9
          '';
        };

        hactar = {
          hostNames = hostNames "hactar";
          publicKey = ''
            ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC7y/poo2XLNsxRoEAgIiUnJgbBa0KassSQSghdXWH1N
          '';
        };
      };
    };
  };

  virtualisation.docker.enable = true;
  system.stateVersion = "20.09";
}
