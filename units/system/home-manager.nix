{
  rhizome,
  units,
  inputs,
  ...
}:

let
  inherit (inputs) home-manager;
  homeModules = inputs.self.homeModules;
in

let
  inherit (units.platform) nixos;
in

rhizome.component "home-manager" [
  (nixos.module (
    { lib, ... }:

    {
      imports = [ home-manager.nixosModules.home-manager ];

      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;

        # Extensions every user gets. Aspects are imported per user.
        sharedModules = [
          homeModules.platform
        ];
      };
    }
  ))
]
