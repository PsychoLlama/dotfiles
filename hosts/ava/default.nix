{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.psychollama.settings) username;
in

{
  imports = [ ./hardware-configuration.nix ];

  config = {
    boot.loader.systemd-boot.enable = true;

    # fprintd doesn't play well with swaylock's pam module. It effectively
    # disables password input.
    services.fprintd.enable = lib.mkForce false;

    networking = {
      networkmanager.enable = true;
      hostId = "daf96cd8"; # Random. Required by the ZFS pool.

      useDHCP = false;
      interfaces.wlp0s20f3.useDHCP = true;
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

    /*
      TODO: Automate this.

      use std/iter *

      let displays = swaymsg -t get_outputs -r | from json
      let main = $displays | iter find { $in.name == "eDP-1" }
      let ext = $displays | iter find { $in.model == "LG ULTRAWIDE" }

      let dimensions = {
        x: (($main.rect.width / 2) - ($ext.rect.width / 2) | into int)
        y: -($ext.rect.height | into int)
      }

      let ext_id = $ext | select make model serial | values | str join " "

      $'
      output "($main.name)" position 0 0
      output "($ext_id)" position ($dimensions.x) ($dimensions.y)
      '
    */
    programs.sway.extraConfig = ''
      # Orient displays supporting my external monitor.
      output "eDP-1" position 0 0
      output "LG Electronics LG ULTRAWIDE 404NTLEDA584" position -760 -1440
    '';

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
