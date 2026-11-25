{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "swaybg" [
  (home-manager.module (
    { pkgs, ... }:

    {
      services.swaybg = {
        enable = true;
        package = pkgs.unstable.swaybg;
        image = "attic/images/wallpapers/current";
      };
    }
  ))
]
