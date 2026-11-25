{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "chromium" [
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
]
