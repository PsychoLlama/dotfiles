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
          programs.chromium = {
            enable = lib.mkDefault true;
            package = lib.mkDefault pkgs.unstable.chromium;
          };
        }
      ))
    ];
  };
}
