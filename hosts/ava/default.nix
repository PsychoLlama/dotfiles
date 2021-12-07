{ unstable, ... }:

{
  imports = [ ./hardware-configuration.nix ../common/linux.nix ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  # Network configuration.
  networking = {
    interfaces.wlp0s20f3.useDHCP = true;
    useDHCP = false; # Deprecated option - explicitly opt out.
    firewall.allowedTCPPorts = [ 30000 ];
    firewall.allowedUDPPorts = [ 30000 ];
  };

  # TODO: Manage background images more intelligently.
  # services.xserver.displayManager.lightdm.background = /home/overlord/background-image;

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
    package = unstable.syncthing-cli;

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
        addresses = [ "dynamic" ];
        id = "73NJCWB-2R4WVZC-IJ2IRPF-7XQH7P4-DM43L7L-HE4HV5D-MKU4Z4V-PL5QVQ5";
      };

      phone = {
        addresses = [ "dynamic" ];
        id = "YTUVZSZ-V4TOBKD-SCKD4B6-AOW5TMT-PGCLJO6-7MLGZII-FOYC7JO-LGP62AX";
      };
    };
  };

  system.stateVersion = "20.09";
}
