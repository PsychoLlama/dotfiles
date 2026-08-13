{ config, inputs, ... }:

let
  inherit (inputs) home-manager;
  homeModules = config.flake.homeModules;
in

{
  flake.nixosModules.home-manager =
    { host, lib, ... }:

    {
      imports = [ home-manager.nixosModules.home-manager ];

      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;

        # Everything every user gets; aspects are imported per user.
        sharedModules = [
          homeModules.platform
          homeModules.secrets
          homeModules.editor

          { _module.args.host = host; }
        ];
      };
    };
}
