{
  units,
  inputs,
  self,
  module,
  ...
}:

let
  inherit (inputs) nixpkgs-unstable self;
in

let
  inherit (units.platform) nixos;
in

{
  unit = module.component "unit" {
    config = [
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
  };
}
