{ config, inputs, ... }:

let
  lib = import ../../lib inputs;

  # Modules opt in with a `.mod.nix` suffix. Plain `.nix` files are free to be
  # helpers, data, or libraries.
  importModules = inputs.import-tree.filter (lib.hasSuffix ".mod.nix");
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
      # `nixos` and `generic` have no platform extensions today. Add the output
      # alongside a `platform/` directory when they do.
      generic.configs = importModules ../generic/psychollama;

      editor = {
        platform = importModules ../editor/platform;
        configs = importModules ../editor/psychollama;
      };

      homeManager = {
        platform = importModules ../homeManager/platform;
        configs = importModules ../homeManager/psychollama;
      };

      nixos.configs = importModules ../nixos/psychollama;
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
