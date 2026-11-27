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
          services.ssh-agent = {
            enable = true;
            package = pkgs.unstable.openssh;
          };
        }
      ))
    ];
  };
}
