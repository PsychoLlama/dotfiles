{ units, define, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = define.unit {
    provides = [
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
  };
}
