{ config, inputs, ... }:

let
  inherit (config.flake) editorModules homeModules nixosModules;

  profiles = [
    "profiles/full"
    "profiles/home-lab-admin"
    "profiles/linux-desktop"
  ];
in

{
  rhizome.hosts.ava = {
    imports = [
      ../_agents/default.nix
      ../_identity.nix
      ../_theme.nix
      ../_trusted-directories.nix
    ];

    system = "x86_64-linux";

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

    module =
      {
        config,
        host,
        lib,
        ...
      }:

      let
        inherit (host.identity) username name;

        shell = config.home-manager.users.${username}.programs.nushell.package;
      in

      {
        imports = [
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
          inputs.nixpkgs.nixosModules.notDetected
        ]
        ++ map (id: nixosModules.${id}) profiles;

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

        # Important! Keep this in sync with the HM user shell.
        environment.shells = [ shell ];

        users.users.${username} = {
          isNormalUser = true;
          description = name;
          shell = shell;

          extraGroups = [
            "wheel"

            # Serial access, for flashing keyboard firmware.
            "dialout"
          ];
        };

        home-manager.users.${username} =
          { config, ... }:
          {
            imports = map (id: homeModules.${id}) profiles;
            programs.editor.imports = map (id: editorModules.${id}) profiles;

            home.stateVersion = "22.05";

            wayland.windowManager.sway.config.output = {
              # Built in display.
              "eDP-1".position = "1440 2360";

              # External monitor.
              "LG Electronics LG ULTRAWIDE 404NTLEDA584" = {
                # Most of my time is spent reading. Using an ultrawide in
                # portrait looks super weird but wow is it a game changer.
                transform = "90";
                position = "0 0";
              };
            };

            # Where the flake lives on disk, used by `nh os` / `nh home`.
            programs.nh.flake = "${config.home.homeDirectory}/projects/psychollama/dotfiles";
          };

        system.stateVersion = "20.09";
      };
  };
}
