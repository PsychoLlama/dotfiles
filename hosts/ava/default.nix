{ config, lib, ... }:

let
  inherit (config.psychollama.identity) username name;
  shell = config.home-manager.users.${username}.programs.nushell.package;
in

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

    networking.hostId = "daf96cd8"; # Random. Required by the ZFS pool.

    # Important! Keep this in sync with the HM user shell.
    environment.shells = [ shell ];

    users.users.${username} = {
      isNormalUser = true;
      description = name;
      shell = shell;
      extraGroups = [
        # Serial access, for flashing keyboard firmware.
        "dialout"

        "wheel"
      ];
    };

    home-manager.users.${username} =
      { config, ... }:
      {
        home.stateVersion = "22.05";

        wayland.windowManager.sway.config.output = {
          # Built in display.
          "eDP-1".position = "1440 2360";

          # External monitor.
          "LG Electronics LG ULTRAWIDE 404NTLEDA584" = {
            # Most of my time is spent reading. Using an ultrawide in portrait
            # looks super weird but wow is it a game changer.
            transform = "90";
            position = "0 0";
          };
        };

        # Where the flake lives on disk, used by `nh os` / `nh home`.
        programs.nh.flake = "${config.home.homeDirectory}/projects/psychollama/dotfiles";
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
    };

    system.stateVersion = "20.09";
  };
}
