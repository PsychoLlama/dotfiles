{ config, inputs, ... }:

let
  lib = import ../lib inputs;
in

{
  imports = [
    inputs.flake-parts.flakeModules.modules

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
        overlays = lib.attrValues config.flake.overlays;
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

    overlays = {
      latest-packages = import ../lib/overlays/latest-packages.nix inputs;
      packages = import ../lib/overlays/packages.nix inputs;
      vim-plugins = import ../lib/overlays/vim-plugins.nix inputs;
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
