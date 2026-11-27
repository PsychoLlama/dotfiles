{ units, module, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = module.component {
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
