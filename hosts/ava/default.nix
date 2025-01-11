{ lib, pkgs, ... }:

let
  username = "overlord";
in

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;

  networking = {
    networkmanager.enable = true;
    hostId = "daf96cd8"; # Random. Required by the ZFS pool.

    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
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

  # fprintd doesn't play well with swaylock's pam module. It effectively
  # disables password input.
  services.fprintd.enable = lib.mkForce false;

  hardware.bluetooth.enable = true;
  system.stateVersion = "20.09";
}
