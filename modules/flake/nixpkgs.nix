{ config, inputs, ... }:

{
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
}
