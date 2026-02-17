{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.psychollama.settings) username;
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

    networking = {
      networkmanager.enable = true;
      hostId = "daf96cd8"; # Random. Required by the ZFS pool.
    };

    # Important! Keep this in sync with the HM user shell.
    environment.shells = [ shell ];

    users.users.${username} = {
      isNormalUser = true;
      name = "overlord";
      description = "Jesse Gibson";
      shell = shell;
      extraGroups = [
        "networkmanager"
        "podman"
        "wheel"
      ];
    };

    home-manager.users.${username} = {
      home.stateVersion = "22.05";
      home.packages = [ pkgs.man-pages ];

      /*
        TODO: Automate this.

        use std/iter *

        let displays = swaymsg -t get_outputs -r | from json
        let main = $displays | iter find { $in.name == "eDP-1" }
        let ext = $displays | iter find { $in.model == "LG ULTRAWIDE" }

        let d = {
          ext: {
            x: 0
            y: 0
          }
          main: {
            x: (($ext.rect.width / 2) - ($main.rect.width / 2) | into int)
            y: $ext.rect.height
          }
        }

        let ext_id = $ext | select make model serial | values | str join " "

        $'
        output "($ext_id)" position ($d.ext.x) ($d.ext.y)
        output "($main.name)" position ($d.main.x) ($d.main.y)
        '
      */
      wayland.windowManager.sway.config.output = {
        "LG Electronics LG ULTRAWIDE 404NTLEDA584".position = "0 0";
        "eDP-1".position = "760 1440";
      };

      programs.git.settings.user = {
        name = "Jesse Gibson";
        email = "JesseTheGibson@gmail.com";
      };

      psychollama.profiles = {
        full.enable = true;
        linux-desktop.enable = true;
      };
    };

    psychollama = {
      settings.username = "overlord";

      profiles = {
        full.enable = true;
        home-lab-admin.enable = true;
      };
    };

    system.stateVersion = "20.09";
  };
}
