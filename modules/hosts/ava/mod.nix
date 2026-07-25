{
  self,
  global,
  lib,
  ...
}:

# Ava, my primary workstation. Only what cannot be generalized into a preset
# lives here: hardware, disk layout, display geometry, and the identity of the
# person sitting in front of it. See ./README.md for the specs.

let
  inherit (global."${self.identity}") username name;
in

{
  config = {
    "${self.identity}" = {
      username = "overlord";
      name = "Jesse Gibson";
      email = "JesseTheGibson@gmail.com";
    };

    "${self.trusted-directories}".paths = [
      "~/projects/psychollama"
      "~/projects/@scratch"
      "~/projects/retreon"
      "~/projects/ambient-computer"
    ];

    "${self.profiles.nixos}".enable = true;
    "${self.profiles.home-manager}".enable = true;
    "${self.profiles.home-lab-admin}".enable = true;
  };

  # Takes `config` for the login shell: it has to match the package
  # home-manager installs, which is only knowable from the NixOS eval.
  platforms.nixos =
    { config, ... }:

    let
      shell = config.home-manager.users.${username}.programs.nushell.package;
    in

    # A root-class platform block merges into the live fixpoint and so cannot
    # declare `imports`. The generated hardware description merges in instead.
    lib.mkMerge [
      (import ./hardware-configuration.nix)

      {
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

        # Important! Keep this in sync with the HM user shell.
        environment.shells = [ shell ];

        users.users.${username} = {
          isNormalUser = true;
          description = name;
          shell = shell;
          extraGroups = [
            "dialout"
            "networkmanager"
            "podman"
            "wheel"
          ];
        };

        home-manager.users.${username} =
          { config, pkgs, ... }:
          {
            home.stateVersion = "22.05";
            home.packages = [ pkgs.man-pages ];

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

        system.stateVersion = "20.09";
      }
    ];
}
