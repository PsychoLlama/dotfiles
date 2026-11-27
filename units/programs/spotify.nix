{ units, define, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = define.unit {
    config = [
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
