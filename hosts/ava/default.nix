{ config, nixpkgs-unstable, lib, inputs, system, ... }:

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

  nix = {
    trustedUsers = [ config.dotfiles.user.account ];

    extraOptions = ''
      builders-use-substitutes = true
    '';

    registry.pkgs.flake = inputs.nixpkgs-unstable;
    nixPath = [ "nixpkgs=${inputs.nixpkgs-unstable}" ];
  };

  services.openssh.hostKeys = [{
    type = "ed25519";
    path = "/root/.ssh/nixops_deploy";
    comment = "NixOps deploy key";
  }];

  environment = {
    systemPackages = with nixpkgs-unstable; [ borgbackup radare2 ];
    variables.BORG_REPO = "/mnt/borg";
  };

  # TODO: Migrate to home-manager.
  dotfiles = {
    kitchen-sink.enable = true;

    user = {
      account = "overlord";
      fullName = "Jesse Gibson";
    };

    desktop.sway.inputs = {
      "2:7:SynPS/2_Synaptics_TouchPad" = { natural_scroll = "enabled"; };
      "1:1:AT_Translated_Set_2_keyboard" = { repeat_delay = 200; };
    };

  };

  # Orient the external monitor centered above the built-in screen.
  environment.etc."sway/config.d/outputs".text = ''
    output eDP-1 pos 0 0 mode 1920x1080
    output HDMI-A-1 pos -760 -1440
  '';

  services.zfs = {
    trim.enable = true;
    autoScrub.enable = true;
    autoSnapshot = {
      enable = true;
      flags = "-k -p --utc";
    };
  };

  services.syncthing = {
    enable = true;
    package = nixpkgs-unstable.syncthing;

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
        id = "YOTWLUU-HU6VAII-DJFNJCT-QBVRGD7-EM37VVL-WNB2HFW-5YFWCUM-INTE4AI";
      };

      phone = {
        addresses = [ "dynamic" ];
        id = "G6MC3RD-GQZ6MUT-MCAOAWP-5JQZTPE-6IEACQV-PWXRW23-KIPCLL2-UQVKLAU";
      };
    };

    extraOptions = {
      options.urAccepted = 3;
      gui.theme = "dark";
    };
  };

  programs.ssh = {
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

  home-manager.users.${config.dotfiles.user.account} = {
    imports = [ inputs.self.nixosModules.home-manager ];

    programs.git = {
      userName = "Jesse Gibson";
      userEmail = "JesseTheGibson@gmail.com";
    };

    presets = {
      desktop-environment.enable = true;
      terminal-environment.enable = true;
      fonts.enable = true;
    };
  };

  networking.firewall.allowedTCPPorts = [ 4444 ];

  # fprintd doesn't play well with swaylock's pam module. It effectively
  # disables password input.
  services.fprintd.enable = lib.mkForce false;

  system.stateVersion = "20.09";
}
