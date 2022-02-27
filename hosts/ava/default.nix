{ config, unstable, ... }:

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
  };

  services.openssh.hostKeys = [{
    type = "ed25519";
    path = "/root/.ssh/nixops_deploy";
    comment = "NixOps deploy key";
  }];

  environment = {
    systemPackages = [ unstable.borgbackup ];
    variables.BORG_REPO = "/mnt/borg";
  };

  dotfiles = {
    kitchen-sink.enable = true;

    user = {
      account = "overlord";
      fullName = "Jesse Gibson";
    };

    desktop.sway.inputs = {
      "2:7:SynPS/2_Synaptics_TouchPad" = { natural_scroll = "enabled"; };
      "1:1:AT_Translated_Set_2_keyboard" = { repeat_delay = 250; };
    };
  };

  # Orient the external monitor centered above the built-in screen.
  environment.etc."sway/config.d/outputs".text = ''
    output HDMI-A-1 pos 0 0
    output eDP-1 pos 760 1440
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
    package = unstable.syncthing;

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

  programs.ssh.extraConfig = ''
    CanonicalizeHostname yes
    CanonicalDomains host.selfhosted.city
    CanonicalizeMaxDots 0

    Host *.host.selfhosted.city
    User admin
  '';

  system.stateVersion = "20.09";
}
