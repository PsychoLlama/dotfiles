{ config, inputs, ... }:

let
  inherit (inputs) home-manager;
  homeModules = config.flake.homeModules;
in

{
  exports.nixos =
    { host, lib, ... }:

    {
      imports = [ home-manager.nixosModules.home-manager ];

      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;

        # Extensions every user gets. Aspects are imported per user.
        sharedModules = [
          homeModules.platform

          { _module.args.host = host; }
        ];
      };
    };
}
