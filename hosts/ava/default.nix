{ unstable, ... }:

{
  imports = [ ./hardware-configuration.nix ../common/linux.nix ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    supportedFilesystems = [ "zfs" ];
  };

  # Network configuration.
  networking = {
    interfaces.wlp0s20f3.useDHCP = true;
    useDHCP = false; # Deprecated option - explicitly opt out.
    hostId = "daf96cd8";
  };

  nix = {
    distributedBuilds = true;

    # Two Raspberry Pis. I use these with NixOps to deploy my home-lab:
    # https://github.com/PsychoLlama/home-lab/
    buildMachines = [
      {
        hostName = "tron.host.selfhosted.city";
        sshUser = "root";
        system = "aarch64-linux";
        sshKey = "/home/overlord/.ssh/remote_builder";
      }
      {
        hostName = "clu.host.selfhosted.city";
        sshUser = "root";
        system = "aarch64-linux";
        sshKey = "/home/overlord/.ssh/remote_builder";
      }
    ];
  };

  services.openssh.hostKeys = [{
    type = "ed25519";
    path = "/home/overlord/.ssh/remote_builder";
    comment = "NixOps deploy key";
  }];

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
        addresses = [ "tcp://hactar" ];
        id = "YOTWLUU-HU6VAII-DJFNJCT-QBVRGD7-EM37VVL-WNB2HFW-5YFWCUM-INTE4AI";
      };

      phone = {
        addresses = [ "dynamic" ];
        id = "YTUVZSZ-V4TOBKD-SCKD4B6-AOW5TMT-PGCLJO6-7MLGZII-FOYC7JO-LGP62AX";
      };
    };

    extraOptions = {
      options.urAccepted = 3;
      gui.theme = "dark";
    };
  };

  system.stateVersion = "20.09";
}
