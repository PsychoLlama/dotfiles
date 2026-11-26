{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "gh" {
  unit = rhizome.component [
    (home-manager.module (
      {
        config,
        lib,
        pkgs,
        ...
      }:

      {
        programs.gh = {
          enable = lib.mkDefault true;
          package = pkgs.unstable.gh;

          settings = {
            git_protocol = "ssh";
          };
        };
      }
    ))
  ];
}
