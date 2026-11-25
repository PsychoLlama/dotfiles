{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "spotify" [
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
]
