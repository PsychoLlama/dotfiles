{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "spotify" {
  unit = rhizome.component [
    (home-manager.module (
      {
        config,
        lib,
        pkgs,
        ...
      }:

      {
        home.packages = [ pkgs.unstable.spotify ];
      }
    ))
  ];
}
