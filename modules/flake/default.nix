{ config, inputs, ... }:

let
  lib = import ../../lib inputs;
  inherit (inputs) import-tree;
in

{
  imports = [
    inputs.flake-parts.flakeModules.modules

    ./den.nix
    ./hosts
    ./overlays
    ./packages.nix
    ./shell.nix
    ./templates.nix
  ];

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

    # Every `.nix` file in these trees is a module. Helpers, data, and libraries
    # opt out with an `_` prefix, which import-tree ignores by default.
    #
    # `nixos` and `generic` have no platform extensions today. Add the output
    # alongside a `platform/` directory when they do.
    modules = {
      generic.configs = import-tree ../generic/psychollama;

      editor = {
        platform = import-tree ../editor/platform;
        configs = import-tree ../editor/psychollama;
      };

      homeManager = {
        platform = import-tree ../homeManager/platform;
        configs = import-tree ../homeManager/psychollama;
      };

      nixos.configs = import-tree ../nixos/psychollama;
    };
  };
}
