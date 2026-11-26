{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "chromium" {
  unit = rhizome.component [
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
}
