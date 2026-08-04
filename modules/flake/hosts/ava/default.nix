{ lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  config = {
    boot.loader.systemd-boot = {
      enable = true;
      configurationLimit = 5;
    };

    # fprintd doesn't play well with swaylock's pam module. It effectively
    # disables password input.
    services.fprintd.enable = lib.mkForce false;

    hardware.keyboard.qmk = {
      enable = true;
      keychronSupport = true;
    };

    networking = {
      networkmanager.enable = true;
      hostId = "daf96cd8"; # Random. Required by the ZFS pool.
    };

    psychollama = {
      identity = {
        username = "overlord";
        name = "Jesse Gibson";
        email = "JesseTheGibson@gmail.com";
      };

      trusted-directories = [
        "~/projects/psychollama"
        "~/projects/@scratch"
        "~/projects/retreon"
        "~/projects/ambient-computer"
      ];

      profiles = {
        full.enable = true;
        home-lab-admin.enable = true;
      };
    };

    system.stateVersion = "20.09";
  };
}
