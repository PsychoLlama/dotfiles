{ inputs, ... }:

let
  inherit (inputs) nixpkgs-unstable self;
in

{
  exports.nixos =
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
    };
}
