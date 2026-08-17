{ config, ... }:

let
  inherit (config.flake) nixosModules;
in

{
  flake.nixosModules.default = {
    imports = [
      nixosModules.home-manager
      nixosModules.nix-daemon
      nixosModules.package-set
      nixosModules.platform
      nixosModules.secrets
    ];
  };

  rhizome.defaults.node = {
    imports = [ nixosModules.default ];
  };
}
