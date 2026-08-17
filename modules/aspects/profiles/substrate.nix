{ config, inputs, ... }:

let
  inherit (inputs) agenix;
  inherit (config.flake) nixosModules;
in

{
  imports = [
    ../system/home-manager.nix
    ../system/nix-daemon.nix
    ../system/package-set.nix
  ];

  exports = {
    nixos.imports = [
      nixosModules.platform
      agenix.nixosModules.default
    ];

    homeManager.imports = [ agenix.homeManagerModules.default ];
  };
}
