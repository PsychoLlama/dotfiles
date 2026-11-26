{
  rhizome,
  units,
  inputs,
  ...
}:

let
  inherit (inputs) nixpkgs-unstable self;
in

let
  inherit (units.platform) nixos;
in

rhizome.unit "nix-daemon" {
  unit = rhizome.component [
    (nixos.module (
      { pkgs, ... }:

      {
        nix = {
          package = pkgs.nixVersions.latest;

          registry = {
            dotfiles.flake = self;
            unstable.flake = nixpkgs-unstable;
          };

          settings = {
            experimental-features = "nix-command flakes";
            flake-registry = null; # Disable default listings.
          };
        };
      }
    ))
  ];
}
