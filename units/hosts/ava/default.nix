# My personal laptop and primary development machine.
{
  rhizome,
  units,
  inputs,
  ...
}:

let
  inherit (units.platform)
    home-manager
    nixos
    theme
    user
    ;

  inherit (units) profiles;

  # The login, and what only this machine wants in it.
  account = rhizome.component "ava.account" {
    uses = { inherit (user) identity; };

    config =
      { identity }:
      [
        (nixos.module (
          { config, ... }:

          let
            shell = config.home-manager.users.${identity.username}.programs.nushell.package;
          in

          {
            # Important! Keep this in sync with the HM user shell.
            environment.shells = [ shell ];

            users.users.${identity.username} = {
              isNormalUser = true;
              description = identity.name;
              shell = shell;

              extraGroups = [
                "wheel"

                # Serial access, for flashing keyboard firmware.
                "dialout"
              ];
            };
          }
        ))

        (home-manager.module (
          { config, ... }:

          {
            home.stateVersion = "22.05";

            wayland.windowManager.sway.config.output = {
              # Built in display. Sits below the external, horizontally centered:
              # (3440 - 1920) / 2 = 760.
              "eDP-1".position = "760 1440";

              # External monitor.
              "LG Electronics LG ULTRAWIDE 404NTLEDA584" = {
                transform = "normal";
                position = "0 0";
              };
            };

            # Where the flake lives on disk, used by `nh os` / `nh home`.
            programs.nh.flake = "${config.home.homeDirectory}/projects/psychollama/dotfiles";
          }
        ))
      ];
  };

  hardware = rhizome.component "ava.hardware" [
    (nixos.module (
      { lib, ... }:

      {
        imports = [
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
          inputs.nixpkgs.nixosModules.notDetected
          ./_hardware-configuration.nix
        ];

        boot.loader.systemd-boot = {
          enable = true;
          configurationLimit = 5;
        };

        # Enabled by nixos-hardware. It breaks login. The pam module takes
        # over and password entry stops working.
        services.fprintd.enable = lib.mkForce false;

        hardware.keyboard.qmk = {
          enable = true;
          keychronSupport = true;
        };

        networking.hostId = "daf96cd8"; # Random. Required by the ZFS pool.

        system.stateVersion = "20.09";
      }
    ))
  ];
in

nixos.host
  {
    name = "ava";
    system = "x86_64-linux";
  }
  [
    hardware
    (theme.palette theme.palettes.one-dark)

    (home-manager.user
      {
        username = "overlord";
        name = "Jesse Gibson";
        email = "JesseTheGibson@gmail.com";
      }
      [
        account

        (map user.trusted-directory [
          "~/projects/psychollama"
          "~/projects/@scratch"
          "~/projects/retreon"
          "~/projects/ambient-computer"
        ])

        profiles.substrate
        profiles.full
        profiles.home-lab-admin
        profiles.linux-desktop
      ]
    )
  ]
