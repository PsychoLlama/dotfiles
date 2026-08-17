{ config, inputs, ... }:

let
  inherit (inputs) nixpkgs-unstable;
  overlays = config.flake.overlays;
in

{
  exports.nixos =
    { lib, ... }:

    {
      nixpkgs = {
        overlays = [
          overlays.unstable-packages
          overlays.custom-packages
          overlays.custom-patches
          overlays.vim-plugins
        ];

        config = {
          # Unfree licenses. To be replaced with libre alternatives.
          allowUnfreePredicate =
            pkg:
            lib.elem (lib.getName pkg) [
              "claude-code-bin"
              "spotify"
            ];
        };

        # Pin `<nixpkgs>` and `flake:nixpkgs` to match system packages.
        flake = {
          source = lib.mkForce nixpkgs-unstable; # Stable is dumb. Live a little.
          setNixPath = lib.mkForce true;
          setFlakeRegistry = true;
        };
      };
    };
}
