{ config, inputs, ... }:

let
  lib = import ../lib inputs;
in

{
  imports = [
    inputs.flake-parts.flakeModules.modules

    ./overlays
    ./packages.nix
    ./shell.nix
    ./templates.nix
  ];

  systems = import inputs.systems;

  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;

        overlays = [
          config.flake.overlays.unstable-packages
          config.flake.overlays.custom-packages
          config.flake.overlays.vim-plugins
        ];
      };
    };

  flake = {
    lib = lib.dotfiles;

    modules = {
      generic.universal = {
        imports = lib.dotfiles.discoverNixFiles {
          directory = ../platforms/universal/modules;
        };
      };

      editor = {
        platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ../platforms/editor/modules;
            exclude = [ ../platforms/editor/modules/psychollama ];
          };
        };

        configs = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ../platforms/editor/modules/psychollama;
          };
        };
      };

      homeManager = {
        platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ../platforms/home-manager/modules;
            exclude = [ ../platforms/home-manager/modules/psychollama ];
          };
        };

        configs = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ../platforms/home-manager/modules/psychollama;
          };
        };
      };

      nixos = {
        platform = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ../platforms/nixos/modules;
            exclude = [ ../platforms/nixos/modules/psychollama ];
          };
        };

        configs = {
          imports = lib.dotfiles.discoverNixFiles {
            directory = ../platforms/nixos/modules/psychollama;
          };
        };
      };
    };

    nixosConfigurations = lib.dotfiles.hosts.nixos {
      ava = [
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
        inputs.nixpkgs.nixosModules.notDetected
        ../hosts/ava
      ];
    };
  };
}
