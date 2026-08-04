{ config, inputs, ... }:

let
  lib = import ../../lib inputs;

  # Modules opt in with a `.mod.nix` suffix. Plain `.nix` files are free to be
  # helpers, data, or libraries.
  importModules = inputs.import-tree.filter (lib.hasSuffix ".mod.nix");

  # The opinionated `psychollama.*` configs are exposed separately.
  importPlatform = importModules.filterNot (lib.hasInfix "/psychollama/");
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
          config.flake.overlays.custom-patches
          config.flake.overlays.vim-plugins
        ];
      };
    };

  flake = {
    lib = lib.dotfiles;

    modules = {
      generic.universal = importModules ../../platforms/universal/modules;

      editor = {
        platform = importPlatform ../../platforms/editor/modules;
        configs = importModules ../../platforms/editor/modules/psychollama;
      };

      homeManager = {
        platform = importPlatform ../../platforms/home-manager/modules;
        configs = importModules ../../platforms/home-manager/modules/psychollama;
      };

      nixos = {
        platform = importPlatform ../../platforms/nixos/modules;
        configs = importModules ../../platforms/nixos/modules/psychollama;
      };
    };

    nixosConfigurations = lib.dotfiles.hosts.nixos {
      ava = [
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
        inputs.nixpkgs.nixosModules.notDetected
        ../../hosts/ava
      ];
    };
  };
}
