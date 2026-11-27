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
  };
}
