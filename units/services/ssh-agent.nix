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
          services.ssh-agent = {
            enable = true;
            package = pkgs.unstable.openssh;
          };
        }
      ))
    ];
  };
}
