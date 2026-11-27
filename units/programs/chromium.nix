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
          programs.chromium = {
            enable = lib.mkDefault true;
            package = lib.mkDefault pkgs.unstable.chromium;
          };
        }
      ))
    ];
  };
}
